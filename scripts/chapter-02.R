
library(tidyverse)
library(rsample)

ames <- AmesHousing::make_ames()

churn <- modeldata::attrition %>% 
    as_tibble()

# Splitting with rsample --------------------------------------------------

set.seed(123)

split_1 <- initial_split(
    ames, 
    prop = 0.7)

train_1 <- training(split_1)
test_1 <- testing(split_1)
















