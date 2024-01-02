# Hospital Admissions Data from https://www.kaggle.com/datasets/ashishsahani/hospital-admissions-data/

# Load libraries and data ----
library(tidymodels)
library(readr)
library(lubridate)

ha_data <- read_csv("HDHI Admission data.csv")

# Get data of interest
# This dataset has a lot of extra data about characteristics of the patients medical status
# For this, we will look at length of stay, date of admission, admission type, and outcome type
# Dates are in a mix of mdy and dmy, so convert to match

# Define variables not deemed important:
non_vip <- c('sno', 'rural', 'month year', 'duration of intensive unit stay', 'smoking', 'alcohol', 'dm', 'htn', 'cad', 'prior cmp', 'ckd', 'glucose', 'raised cardiac enzymes', 'severe anaemia', 'anaemia', 'stable angina', 'acs', 'stemi', 'atypical chest pain', 'heart failure', 'hfref', 'hfnef', 'valvular', 'chb', 'sss', 'aki', 'cva infract', 'cva bleed', 'af', 'vt', 'psvt', 'congenital', 'uti', 'neuro cardiogenic syncope', 'orthostatic', 'infective endocarditis', 'dvt', 'pulmonary embolism', 'chest infection')

ha_vip_data_clean <- ha_data |>
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
       OUTCOME)
  ) |>
  rename_with(
    tolower
  ) |>
  select(-any_of(non_vip))

# Create train and test sets
set.seed(123)

ha_vip_split <- initial_split(ha_vip_data_clean, prop = .8)
ha_vip_train <- training(ha_vip_split)
ha_vip_test <- testing(ha_vip_split)

ha_vip_kfolds <- vfold_cv(ha_vip_train)

# Set up recipe
# We will make "outcome" the variable of interest
ha_vip_recipe <- recipe(outcome ~ ., data = ha_vip_train) |>
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
rforest_spec_vip <-
  rand_forest(mtry = tune(), trees = 500) %>%
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

rforest_wflow_vip <-
  workflow() %>%
  add_recipe(ha_vip_recipe) %>%
  add_model(rforest_spec_vip)

# Specify the tuning grid for mtry
mtry_values_vip <- seq(1, min(20, ncol(ha_vip_train) - 1), by = 1)  # Adjust the range as needed

# Train and evaluate the model using cross-validation with specified mtry values
fit_vip <- rforest_wflow_vip %>%
  tune_grid(resamples = vfold_cv(ha_vip_train, v = 5),
            grid = expand.grid(mtry = mtry_values_vip))

# Get the best model ----
best_model_vip <- select_best(fit_vip, "accuracy")

# Finalize workflow and model ----
final_workflow_vip <- rforest_wflow_vip |>
  finalize_workflow(best_model_vip)

final_model_vip <- fit(final_workflow_vip, ha_vip_data_clean)

# Assess final model ----
test_results_vip <-
  final_workflow_vip %>%
  last_fit(split = ha_vip_split)

ha_vip_predictions <- test_results_vip %>%
  collect_predictions()

roc_auc_value_vip <-
  ha_vip_predictions %>%
  roc_auc(truth = outcome, .pred_DAMA:.pred_EXPIRY)

roc_values_vip <-
  ha_vip_predictions %>%
  roc_curve(truth = outcome, .pred_DAMA:.pred_EXPIRY)

roc_plot_vip <- autoplot(roc_values_vip) +
  xlab("True Positive Rate") +
  ylab("False Positive Rate")

# Save Model ----
saveRDS(final_model_vip, file = "ha_vip_model.RDS")

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

vip_class_model <- readRDS("ha_vip_model.RDS")

# Test prediction
# Does not work, next step will be to do a subset model using only variables of importance above
predict(vip_class_model,
        tibble(
          mrn = factor(12345),
          age = 60,
          sex = factor("M"),
          admit_type = factor("E"),
          date_arrival = as_date("2018-10-01"),
          date_discharge = as_date("2018-10-02"),
          n_days = 1,
          `duration of stay` = 5,
          `hb` = 15,
          `tlc` = 15,
          `platelets` = 500,
          `urea` = 15,
          `creatinine` = 2,
          `bnp` = 200,
          `ef` = 50,
          `cardiogenic shock` = 1,
          `shock` = 1
        )
)
