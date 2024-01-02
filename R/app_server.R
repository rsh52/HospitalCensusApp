#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Load data and model into temp environment, then convert to reactive values list
  load("dev/hospital_admissions/ha_vip_env.RData",  temp_env_class <- new.env())
  load("dev/hospital_admissions/ha_long_env.RData",  temp_env_long <- new.env())
  r <- as.list(temp_env_class)
  r <- append(r, as.list(temp_env_long))

  # Load Model
  r$class_model <- readRDS("dev/hospital_admissions/ha_vip_model.RDS")
  r$linreg_model <- readRDS("dev/hospital_admissions/ha_linreg_model.RDS")
  r$arima_model <- readRDS("dev/hospital_admissions/ha_arima_model.RDS")

  r$input <- list()

  r <- do.call("reactiveValues", r)

  mod_class_sidebar_server("class_sidebar_1", r)
  mod_regression_sidebar_server("regression_sidebar_1", r)
  mod_class_server("class_1", r)
  mod_regression_server("regression_1", r)
}
