# Hospital Admissions Data from https://www.kaggle.com/datasets/ashishsahani/hospital-admissions-data/

# Load libraries and data ----
library(tidymodels)
library(readr)
library(lubridate)

ha_data <- read_csv("HDHI Admission data.csv")

# Light EDA ----
any(duplicated(ha_data$`MRD No.`)) # There are duplicated MRD No., SNO is the primary key

# Get data of interest
# This dataset has a lot of extra data about characteristics of the patients medical status
# For this, we will look at length of stay, date of admission, admission type, and outcome type
# Dates are in a mix of mdy and dmy, so convert to match
ha_full_data_clean <- ha_data |>
  mutate(
    mrn = factor(`MRD No.`),
    age = AGE,
    sex = factor(GENDER),
    admit_type = factor(`TYPE OF ADMISSION-EMERGENCY/OPD`),
    date_arrival = as_date(parse_date_time(D.O.A, orders = c("mdy", "dmy"))),
    date_discharge = as_date(parse_date_time(D.O.D, orders = c("mdy", "dmy"))),
    outcome = factor(OUTCOME)
  ) |>
  mutate(
    # Remove character strings saying "EMTPY"
    across(c(GLUCOSE, BNP, EF), ~case_when(. == "EMPTY" ~ NA_character_, TRUE ~ .)),
    across(c(RURAL, SMOKING:CKD, `RAISED CARDIAC ENZYMES`, `SEVERE ANAEMIA`:ORTHOSTATIC),
           ~ factor(.)),
    across(c(HB:BNP, EF), ~as.numeric(.))
  ) |>
  # remove cols we don't need
  select(
    -c(`MRD No.`, D.O.A, D.O.D, GENDER, `TYPE OF ADMISSION-EMERGENCY/OPD`, AGE,
       OUTCOME, )
  ) |>
  rename_with(
    tolower
  )

# Create train and test sets
set.seed(123)

ha_full_split <- initial_split(ha_full_data_clean, prop = .8)
ha_full_train <- training(ha_full_split)
ha_full_test <- testing(ha_full_split)

ha_full_kfolds <- vfold_cv(ha_full_train)

# Set up recipe
# We will make "outcome" the variable of interest
ha_full_recipe <- recipe(outcome ~ ., data = ha_full_train) |>
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
  step_dummy(all_nominal_predictors()) |>
  # impute missing data using knn for lab values
  step_impute_knn(all_numeric_predictors())

# Specify model spec and workflow
rforest_spec_full <-
  rand_forest(mtry = tune(), trees = 500) %>%
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rforest_wflow_full <-
  workflow() %>%
  add_recipe(ha_full_recipe) %>%
  add_model(rforest_spec_full)

# Specify the tuning grid for mtry
mtry_values_full <- seq(1, min(20, ncol(ha_full_train) - 1), by = 1)  # Adjust the range as needed

# Train and evaluate the model using cross-validation with specified mtry values
fit_full <- rforest_wflow_full %>%
  tune_grid(resamples = vfold_cv(ha_full_train, v = 5),
            grid = expand.grid(mtry = mtry_values_full))

# Get the best model ----
best_model_full <- select_best(fit_full, "accuracy")

# Finalize workflow and model ----
final_workflow_full <- rforest_wflow_full |>
  finalize_workflow(best_model_full)

final_model_full <- fit(final_workflow_full, ha_full_data_clean)

# Assess final model ----
test_results_full <-
  final_workflow_full %>%
  last_fit(split = ha_full_split)

ha_full_predictions <- test_results_full %>%
  collect_predictions()

roc_auc_value_full <-
  ha_full_predictions %>%
  roc_auc(truth = outcome, .pred_DAMA:.pred_EXPIRY)

roc_values_full <-
  ha_full_predictions %>%
  roc_curve(truth = outcome, .pred_DAMA:.pred_EXPIRY)

roc_plot_full <- autoplot(roc_values_full) +
  xlab("True Positive Rate") +
  ylab("False Positive Rate")

# Save Model ----
saveRDS(final_model_full, file = "ha_full_model.RDS")

# Check variable importance
# Note: doesnt work without importance specified at tree growth
library(vip)
test_fit <- final_workflow_full %>%
  fit(ha_full_test) %>%
  pluck("fit", "fit")
# Retrieve variable importance
vip(test_fit)

# Variable importance:
# - duration of stay
# - shock
# - cardiogenic shock
# - tlc
# - urea
# - bnp
# - platelets
# - creatinine
# - hb
# - ef

full_class_model <- readRDS("ha_full_model.RDS")

# Test prediction
# Does not work, next step will be to do a subset model using only variables of importance above
predict(full_class_model,
        tibble(
          mrn = factor(12345),
          age = 60,
          sex = factor("M"),
          admit_type = factor("E"),
          date_arrival = as_date("2018-10-01"),
          date_discharge = as_date("2018-10-02"),
          n_days = 1,
          sno = numeric(0L),
          rural = character(0L),
          `month year` = "2020-01-01",
          `duration of stay` = 5,
          `duration of intensive unit stay` = numeric(0L),
          `smoking` = factor(0),
          `alcohol` = factor(0),
          `dm` = factor(0),
          `htn` = factor(0),
          `cad` = factor(0),
          `prior cmp` = factor(0),
          `ckd` = factor(0),
          `hb` = 15,
          `tlc` = 15,
          `platelets` = 500,
          `glucose` = 0L,
          `urea` = 15,
          `creatinine` = 2,
          `bnp` = 200,
          `raised cardiac enzymes` = factor(0),
          `ef` = 50,
          `severe anaemia` = factor(0),
          `anaemia` = factor(0),
          `stable angina` = factor(0),
          `acs` = factor(0),
          `stemi` = factor(0),
          `atypical chest pain` = factor(0),
          `heart failure` = factor(0),
          `hfref` = factor(0),
          `hfnef` = factor(0),
          `valvular` = factor(0),
          `chb` = factor(0),
          `sss` = factor(0),
          `aki` = factor(0),
          `cva infract` = factor(0),
          `cva bleed` = factor(0),
          `af` = factor(0),
          `vt` = factor(0),
          `psvt` = factor(0),
          `congenital` = factor(0),
          `uti` = factor(0),
          `neuro cardiogenic syncope` = factor(0),
          `orthostatic` = factor(0),
          `infective endocarditis` = 0L,
          `dvt` = 0L,
          `cardiogenic shock` = 1,
          `shock` = 1,
          `pulmonary embolism` = 0L,
          `chest infection` = 0L
          )
        )
