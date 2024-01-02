# Using example from https://www.tidymodels.org/learn/work/case-weights/

# Case Weights Model ----
library(tidymodels)
data(Chicago)

Chicago <- Chicago %>%
  select(ridership, date, one_of(stations))


weights_from_dates <- function(x, ref) {
  if_else(
    condition = x >= ref,
    true = 1,     # <- Notice that I'm setting any future weight to 1.
    false = 0.999 ^ as.numeric(difftime(ref, x, units = "days"))
  )
}

Chicago <- Chicago %>%
  mutate(weight = weights_from_dates(date, "2016-01-01"),
         weight = importance_weights(weight))


Chicago_train <- Chicago %>% filter(date < "2016-01-01")
Chicago_test <- Chicago %>% filter(date >= "2016-01-01")

base_recipe <-
  recipe(ridership ~ ., data = Chicago_train) %>%
  # Create date features
  step_date(date) %>%
  step_holiday(date, keep_original_cols = FALSE) %>%
  # Remove any columns with a single unique value
  step_zv(all_predictors()) %>%
  # Normalize all the numerical features
  step_normalize(all_numeric_predictors()) %>%
  # Perform PCA to reduce the correlation bet the stations
  step_pca(all_numeric_predictors(), threshold = 0.95)

lm_spec <-
  linear_reg() %>%
  set_engine("lm")

lm_wflow <-
  workflow() %>%
  add_case_weights(weight) %>%
  add_recipe(base_recipe) %>%
  add_model(lm_spec)

lm_fit <- fit(lm_wflow, data = Chicago_train)

saveRDS(lm_fit, file = "dev/model.RDS")

# Vetiver model -----
# library(vetiver)
#
# v <- vetiver_model(lm_fit, model_name = "case_weights")
