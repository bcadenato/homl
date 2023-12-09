
library(tidyverse)
library(tidymodels)

data(Sacramento)

# Create a training set without ANTELOPE as city value 
# and a test set with ANTELOPE as a city value

sacr_tr <- Sacramento %>% 
    filter(! city %in% c("ANTELOPE"))

sacr_te <- Sacramento %>% 
    filter(city %in% c("ANTELOPE"))

# Create a workflow that uses step_novel in the recipe, and fit the model

rec <- recipe(
    price ~ city,
    data = sacr_tr) %>% 
    step_novel(city)

mod <- linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")

wf <- workflow() %>% 
    add_recipe(rec) %>% 
    add_model(mod)

wf_fit <- wf %>% 
    fit(sacr_tr)

# The model cannot predict on the test set because it had not seen ANTELOPE before as a value, 
# even if ANTELOPE is a level it knows

wf_pred <- wf_fit %>% 
    predict(sacr_te)

# Remove ANTELOPE level from city set of levels in the training set
# and refit the model with the resulting training set

sacr_tr_fct <- sacr_tr %>% 
    mutate(
        city = city %>% 
            as.character() %>% 
            factor())

rec_fct <- recipe(
    price ~ city,
    data = sacr_tr_fct) %>% 
    step_novel(city)

wf_fct <- wf %>% 
    update_recipe(
        rec_fct)

wf_fct_fit <- wf_fct %>% 
    fit(sacr_tr_fct)

# The model can predict without errors even if it cannot make a prediction
# ANTELOPE level is converted to `new` level and the model can manage it

wf_fct_pred <- wf_fct_fit %>% 
    predict(sacr_te)

# If the training set doesn't have ANTELOPE as a level, step_novel can
# transform it to the value `new` as expected

wf_fit %>% 
    extract_recipe() %>% 
    bake(sacr_te)

wf_fct_fit %>% 
    extract_recipe() %>% 
    bake(sacr_te)
















