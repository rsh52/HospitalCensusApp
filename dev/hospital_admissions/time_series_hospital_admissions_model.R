
# Hospital Admissions Data from https://www.kaggle.com/datasets/ashishsahani/hospital-admissions-data/

# Using same dataset as other model, but pivoted to look at patients still in hospital to define a "census"

# Load libraries and data ----
library(tidymodels)
library(readr)
library(lubridate)
library(modeltime)

ha_data <- read_csv("dev/hospital_admissions/HDHI Admission data.csv")

ha_long <- ha_data |>
  transmute(
    mrn = factor(`MRD No.`),
    age = AGE,
    sex = factor(GENDER),
    admit_type = factor(`TYPE OF ADMISSION-EMERGENCY/OPD`),
    date_arrival = as_date(parse_date_time(D.O.A, orders = c("mdy", "dmy"))),
    date_discharge = as_date(parse_date_time(D.O.D, orders = c("mdy", "dmy"))),
    n_days = as.numeric(difftime(date_discharge, date_arrival, units = "days")),
    outcome = factor(OUTCOME)
  ) |>
  filter(n_days >= 0) |>
  dplyr::rowwise() |>
  mutate(
    dates_between = list(seq.Date(date_arrival, date_discharge, by = "day"))
  ) |>
  pivot_longer(
    cols = dates_between,
    names_to = NULL,
    values_to = "date"
  ) |>
  tidyr::unnest(date) |>
  mutate(
    disposition = case_when(date_discharge == date ~ outcome,
                            TRUE ~ "IN HOSPITAL"),
    disposition = factor(disposition)
  )

ha_long <- ha_long |>
  filter(disposition == "IN HOSPITAL") |>
  summarise(
    .by = c(date),
    count = n()
  )

# Create train and test sets
set.seed(123)

ha_long_split <- initial_time_split(ha_long, prop = .8)
ha_long_train <- training(ha_long_split)
ha_long_test <- testing(ha_long_split)

ha_long_kfolds <- vfold_cv(ha_long_train)

ha_long_recipe <- recipe(count ~ . , ha_long_train) |>
  step_date(date)

# Linear Regression ----
# Set linear regression specification
linreg_spec <- linear_reg() |>
  set_engine("lm")

# Worfklow spec
linreg_wflow <-
  workflow() |>
  add_recipe(ha_long_recipe) |>
  add_model(linreg_spec)

# Fit simple model
linreg_fit <- linreg_wflow |>
  tune_grid(vfold_cv(ha_long_train, v = 5),
            expand.grid())

# Get best model
best_ha_linreg_model <- select_best(linreg_fit, "rmse")

final_ha_linreg_workflow <- linreg_wflow |>
  finalize_workflow(best_ha_linreg_model)

# Get final model
final_ha_linreg_model <- fit(final_ha_linreg_workflow, ha_long)

saveRDS(final_ha_linreg_model, file = "dev/hospital_admissions/ha_linreg_model.RDS")

# Set auto arima specification
arima_spec <- arima_reg() |>
  set_engine("auto_arima")

# Worfklow spec
arima_wflow <-
  workflow() |>
  add_recipe(ha_long_recipe) |>
  add_model(arima_spec)

# Fit arima model
arima_fit <- arima_wflow |>
  fit(ha_long_train)

final_ha_arima_workflow <- arima_wflow |>
  finalize_workflow(arima_fit)

# Get final model
final_ha_arima_model <- fit(final_ha_arima_workflow, ha_long)

saveRDS(final_ha_arima_model, file = "dev/hospital_admissions/ha_arima_model.RDS")

# Test prediction
# predict(final_ha_arima_model, tibble(date = as_date("2023-01-01")))
