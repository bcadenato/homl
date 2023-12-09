
# Load libraries ----------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

ames <- AmesHousing::make_ames()

set.seed(123)

ames_split_01 <- ames %>% rsample::initial_split(
    prop = 0.7,
    strata = Sale_Price)

ames_train_01 <- ames_split_01 %>% 
    rsample::training()

ames_test_01 <- ames_split_01 %>% 
    rsample::testing()

# Process model step by step ----------------------------------------------

# Recipe

pcr_rec_01 <- recipes::recipe(
    Sale_Price ~ .,
    data = ames_train_01)

pcr_rec_02 <- pcr_rec_01 %>% 
    recipes::step_novel(
        recipes::all_factor_predictors())

pcr_rec_03 <- pcr_rec_01 %>% 
    recipes::step_dummy(
        recipes::all_factor_predictors())

pcr_rec_04 <- pcr_rec_03 %>% 
    recipes::step_nzv(
        recipes::all_predictors())

pcr_rec_05 <- pcr_rec_04 %>% 
    recipes::step_normalize(
        recipes::all_numeric_predictors())

pcr_rec_06 <- pcr_rec_05 %>% 
    recipes::step_pca(
        recipes::all_numeric_predictors(),
        num_comp = 5)

# Prepare the recipe
# Calculate statistics out of training data

pcr_rec_07 <- pcr_rec_06 %>% 
    recipes::prep()

# Explore recipe

pcr_rec_07 %>% 
    recipes::bake(ames_train_01)

pcr_rec_07 %>% 
    recipes::tidy()

pcr_rec_07 %>% 
    recipes::tidy(4)

# Model

pcr_model_01 <- parsnip::linear_reg()

pcr_model_02 <- pcr_model_01 %>% 
    parsnip::set_engine("lm")

pcr_model_03 <- pcr_model_02 %>% 
    parsnip::set_mode("regression")

# Cross validation
# This is the best approach to evaluate a model without using
# the test data set

pcr_folds_01 <- rsample::vfold_cv(
    data = ames_train_01,
    v = 10)

# Workflow

pcr_wf_01 <- workflows::workflow()

pcr_wf_02 <- pcr_wf_01 %>% 
    workflows::add_recipe(pcr_rec_06)

pcr_wf_03 <- pcr_wf_02 %>% 
    workflows::add_model(
        pcr_model_03)

# Fit with cross validations ----------------------------------------------

# Fit model with cross validation

pcr_control_resamples_01 <- tune::control_resamples(
    save_pred = TRUE)

pcr_wf_11 <- pcr_wf_03 %>% 
    tune::fit_resamples(
        pcr_folds_01,
        control = pcr_control_resamples_01)

pcr_wf_11 %>% 
    tune::collect_metrics()

pcr_wf_11 %>% 
    tune::collect_predictions()

# Tune grid ---------------------------------------------------------------

pcr_rec_11 <- pcr_rec_05 %>% 
    step_pca(
        all_numeric_predictors(),
        num_comp = tune("n_pca"))

pcr_wf_11 <- workflows::workflow() %>% 
    workflows::add_recipe(pcr_rec_11) %>% 
    workflows::add_model(pcr_model_03)

pcr_param_01 <- pcr_rec_11 %>% 
    hardhat::extract_parameter_set_dials()
    update(
        n_pca = dials::num_comp(range = c(1, 25)))

pcr_grid_01 <- dials::grid_regular(
    pcr_param_01,
    levels = 25)

pcr_control_grid_01 <- tune::control_grid(
    verbose = TRUE)

pcr_wf_12 <- pcr_wf_11 %>% 
    tune::tune_grid(
        resamples = pcr_folds_01,
        grid = pcr_grid_01,
        control = pcr_control_grid_01)

pcr_wf_12 %>% 
    parsnip::autoplot()

pcr_wf_12 %>% 
    tune::collect_metrics()

# Fit the final model and obtain final performance ------------------------

# Finalize the workflow

pcr_rec_21 <- pcr_rec_11 %>% 
    tune::finalize_recipe(
        parameters = pcr_wf_12 %>% 
            tune::show_best("rmse", n = 1))

pcr_wf_21 <- pcr_wf_03 %>% 
    workflows::update_recipe(
        pcr_rec_21)

# Manual fit the model

pcr_wf_22 <- pcr_wf_21 %>% 
    parsnip::fit(
        data = ames_train_01)

# Evaluate fit model

pcr_pred_01 <- pcr_wf_22 %>% 
    predict(
        ames_test_01)

ames_test_01 %>% 
    bind_cols(pcr_pred_01) %>% 
    yardstick::metrics( 
        truth = Sale_Price, 
        estimate = .pred)

# Alternatively use last fit

pcr_wf_31 <- pcr_wf_21 %>% 
    tune::last_fit(
        ames_split_01)

pcr_wf_31 %>% 
    tune::collect_metrics()
















