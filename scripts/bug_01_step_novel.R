library(dplyr)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(rsample)

data(Sacramento, package = "modeldata")

sacr_tr <- Sacramento %>% 
    filter(! city %in% c("RIO_LINDA"))

sacr_te <- Sacramento %>% 
    filter(city %in% c("RIO_LINDA"))

sacr_tr_alt <- sacr_tr %>% 
    mutate(
        city = as.character(city),
        city = factor(
            x = city))

rec <- recipe(city ~ zip, data = sacr_tr) %>%
    step_novel(city)

rec_alt <- recipe(city ~ zip, data = sacr_tr_alt) %>%
    step_novel(city)

rec <- prep(rec, training = sacr_tr)

rec_alt <- prep(rec_alt, training = sacr_tr_alt)

processed <- bake(rec, sacr_te)

processed_alt <- bake(rec_alt, sacr_te)

tibble(old = sacr_te$city, new = processed$city)

tibble(old = sacr_te$city, new = processed_alt$city)



