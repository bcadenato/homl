
# Libraries ---------------------------------------------------------------

library(tidyverse)

data(ames, package = "modeldata")

set.seed(123)

# Exploratory data analysis -----------------------------------------------

ames %>% skimr::skim()

ames %>% 
    group_by(
        Neighborhood) %>% 
    mutate(
        Max_Sale_Price = max(Sale_Price),
        Min_Sale_Price = min(Sale_Price)) %>% 
    ggplot(
        aes(
            y = fct_reorder(Neighborhood, Max_Sale_Price),
            xmin = Min_Sale_Price,
            xmax = Max_Sale_Price,
            x = Sale_Price)) +
    geom_linerange() +
    labs(x = "Sale Price", y = "Neighbourhood") 

ames %>% 
    ggplot(aes(
        x = Total_Bsmt_SF,
        y = Sale_Price)) +
    geom_point() +
    geom_smooth()
        
# Data budget -------------------------------------------------------------

ames_split <- ames %>% 
    rsample::initial_split(
        strata = Sale_Price)

ames_training <- ames_split %>% 
    rsample::training()

ames_test <- ames_split %>% 
    rsample::testing()

ames_resamples <- ames_training %>% 
    rsample::vfold_cv(
        v = 10)

# Preprocessing -----------------------------------------------------------

recipe_01 <- ames_training %>% 
    recipes::recipe(
        formula = Sale_Price ~ .)

recipe_02 <- recipe_01 %>% 
    recipes::step_nzv(
        recipes::all_predictors()) %>% 
    recipes::step_dummy(
        recipes::all_factor_predictors())

recipe_03 <- recipe_02 %>% 
    recipes::step_log(
        recipes::all_outcomes())

recipe_11 <- recipe_03 %>% 
    recipes::step_pca(
        num_comp = tune::tune(id = "n_components"),
        recipes::all_numeric_predictors())

# Explore baked dataset

recipe_prep_01 <- recipe_03 %>% 
    recipes::prep(
        data = ames_training)
    
ames_training_prep_01 <- recipe_prep_01 %>% 
    recipes::bake(
        new_data = ames_training)

# Models ------------------------------------------------------------------

model_01 <- parsnip::linear_reg() %>% 
    parsnip::set_engine("lm") %>% 
    parsnip::set_mode("regression")

model_02 <- parsnip::linear_reg(
        penalty = tune::tune(
            id = "ridge_penalty"),
        mixture = 0) %>% 
    parsnip::set_engine("glmnet") %>% 
    parsnip::set_mode("regression")

model_03 <- parsnip::linear_reg(
        penalty = tune::tune(
            id = "lasso_penalty"),
        mixture = 1) %>% 
    parsnip::set_engine("glmnet") %>% 
    parsnip::set_mode("regression")

model_04 <- parsnip::linear_reg(
        penalty = tune::tune(
            id = "elastic_net_penalty"),
        mixture = tune::tune(
            id = "elastic_net_mixture")) %>% 
    parsnip::set_engine("glmnet") %>% 
    parsnip::set_mode("regression")

# Workflows Set -----------------------------------------------------------

wfs_01 <- workflowsets::workflow_set(
    preproc = list(
        base = recipe_03,
        pca = recipe_11),
    models = list(
        lm = model_01,
        ridge = model_02,
        lasso = model_03,
        elastic_net = model_04))

wfs_02 <- wfs_01 %>% 
    filter(
        wflow_id %>% str_detect("base_") |
        wflow_id %>% str_detect("pca_lm"))

tune_fit <- wfs_02 %>% 
    workflowsets::workflow_map(
        fn = "tune_grid",
        resamples = ames_resamples,
        grid = 10,
        verbose = TRUE)

tune_fit %>% 
    workflowsets::rank_results(
        rank_metric = "rmse",
        select_best = TRUE)

tune_fit %>% 
    autoplot(
        rank_metric = "rmse",
        metric = "rmse",
        select_best = TRUE)

# Last fit ----------------------------------------------------------------

best_results <- tune_fit %>% 
    workflowsets::extract_workflow_set_result(
        "base_lasso") %>% 
    tune::select_best(
        metric = "rmse")
    
lasso_best_results <- tune_fit %>% 
    workflowsets::extract_workflow(
        "base_lasso") %>% 
    tune::finalize_workflow(
        best_results) %>% 
    tune::last_fit(
        ames_split)

lasso_best_results %>% 
    tune::collect_metrics()

lasso_best_results %>% 
    tune::collect_predictions() %>% 
    ggplot(
        aes(
            x = Sale_Price,
            y = .pred)) +
    geom_abline(
        colour = "grey") +
    geom_point(
        alpha = 0.5) +
    tune::coord_obs_pred() + 
    labs(x = "Observed", y = "Predicted")

lasso_best_recipe <- lasso_best_results %>% 
    workflows::extract_recipe()

lasso_best_fit <- lasso_best_results %>% 
    workflows::extract_fit_engine()



















