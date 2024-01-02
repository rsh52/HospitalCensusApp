# Hospital Admissions Data from https://www.kaggle.com/datasets/ashishsahani/hospital-admissions-data/

# Load libraries and data ----
library(tidymodels)
library(readr)
library(lubridate)

ha_data <- read_csv("dev/hospital_admissions/HDHI Admission data.csv")

# Light EDA ----
any(duplicated(ha_data$`MRD No.`)) # There are duplicated MRD No., SNO is the primary key

# Get data of interest
# This dataset has a lot of extra data about characteristics of the patients medical status
# For this, we will look at length of stay, date of admission, admission type, and outcome type
# Dates are in a mix of mdy and dmy, so convert to match
# Create new column n_days for tdiff
ha_data <- ha_data |>
  transmute(
    mrn = `MRD No.`,
    age = AGE,
    sex = factor(GENDER),
    admit_type = factor(`TYPE OF ADMISSION-EMERGENCY/OPD`),
    date_arrival = as_date(parse_date_time(D.O.A, orders = c("mdy", "dmy"))),
    date_discharge = as_date(parse_date_time(D.O.D, orders = c("mdy", "dmy"))),
    n_days = as.numeric(difftime(date_discharge, date_arrival, units = "days")),
    outcome = factor(OUTCOME)
  )

# Create train and test sets
set.seed(123)

ha_split <- initial_split(ha_data, prop = .8)
ha_train <- training(ha_split)
ha_test <- testing(ha_split)

ha_kfolds <- vfold_cv(ha_train)

# Set up recipe
# We will make "outcome" the variable of interest
ha_recipe <- recipe(outcome ~ ., data = ha_train) |>
  # Remove unnecessary features
  step_rm(mrn) |>
  # Create date features
  step_date(date_arrival) |>
  step_date(date_discharge) |>
  # Remove any columns with a single unique value
  step_zv(all_predictors()) |>
  # Normalize all numeric features
  step_normalize(all_numeric_predictors()) |>
  # Normalize all nominal features
  step_dummy(all_nominal_predictors())

# Specify model spec and workflow
rforest_spec <-
  rand_forest(mtry = tune(), trees = 500) %>%
  set_engine("ranger") |>
  set_mode("classification")

rforest_wflow <-
  workflow() %>%
  add_recipe(ha_recipe) %>%
  add_model(rforest_spec)

# Specify the tuning grid for mtry
mtry_values <- seq(1, min(20, ncol(ha_train) - 1), by = 1)  # Adjust the range as needed

# Train and evaluate the model using cross-validation with specified mtry values
fit <- rforest_wflow %>%
  tune_grid(resamples = vfold_cv(ha_train, v = 5),
            grid = expand.grid(mtry = mtry_values))

# Get the best model ----
best_model <- select_best(fit, "accuracy")

# Finalize workflow and model ----
final_workflow <- rforest_wflow |>
  finalize_workflow(best_model)

final_model <- fit(final_workflow, ha_data)

# Assess final model ----
test_results <-
  final_workflow %>%
  last_fit(split = ha_split)

ha_predictions <- test_results %>%
  collect_predictions()

roc_auc_value <-
  ha_predictions %>%
  roc_auc(truth = outcome, .pred_DAMA:.pred_EXPIRY)

roc_values <-
  ha_predictions %>%
  roc_curve(truth = outcome, .pred_DAMA:.pred_EXPIRY)

roc_plot <- autoplot(roc_values) +
  xlab("True Positive Rate") +
  ylab("False Positive Rate")

# Save Model ----
saveRDS(final_model, file = "dev/ha_model.RDS")

# Test prediction
# predict(final_model,
#         tibble(
#           mrn = 12345,
#           age = 60,
#           sex = factor("M"),
#           admit_type = factor("E"),
#           date_arrival = as_date("2018-10-01"),
#           date_discharge = as_date("2018-10-02"),
#           n_days = 1))
