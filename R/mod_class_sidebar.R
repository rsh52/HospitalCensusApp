#' class_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_class_sidebar_ui <- function(id){
  ns <- NS(id)
  sidebar(
    title = "Input Parameters",
    # Min and Max start and end dates in test data
    dateInput(ns("admit_date"), "Admission Date", Sys.Date() - 1),
    dateInput(ns("discharge_date"), "Discharge Date", Sys.Date()),
    radioButtons(ns("admit_type"), "Admission Type",
                 choiceNames = c("Emergency", "Outpatient"),
                 choiceValues = c("E", "O")),
    radioButtons(ns("sex"), "Sex",
                 choiceNames = c("Male" , "Female"),
                 choiceValues = c("M", "F")),
    numericInput(ns("age"), "Age", min = 0, step = 1, value = 50),
    numericInput(ns("stay"), "Duration of Stay", min = 1, step = 1, value = 1),
    radioButtons(ns("shock"), "Shock", choices = c(1, 0), selected = 0),
    radioButtons(ns("card_shock"), "Cardiogenic Shock", choices = c(1, 0), selected = 0),
    # The following default values are based on median values from the data
    numericInput(ns("tlc"), label = "TLC", value = 10.1, min = 0),
    numericInput(ns("urea"), label = "Urea", value = 35, min = 0),
    numericInput(ns("bnp"), label = "BNP", value = 470, min = 0),
    numericInput(ns("platelets"), label = "Platelets", value = 226, min = 0),
    numericInput(ns("creatinine"), label = "Creatinine", value = 1, min = 0),
    numericInput(ns("hb"), label = "HB", value = 470, min = 0),
    numericInput(ns("ef"), label = "Ejection Fraction", value = 42, min = 0),
    actionButton(ns("run_pred_class"), label = "Run Prediction",
                 lib = "fontawesome", icon = icon("lightbulb")),
    # Conditional action button implemented for developer debugging session
    # Using SHINY_PORT checks for whether session is local or on RSConnect
    if (Sys.getenv("SHINY_PORT") == "") {
      actionButton(ns("browser"), label = "Initiate browser()")
    }
  )

}

#' class_sidebar Server Functions
#'
#' @noRd
mod_class_sidebar_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$run_pred_class, {
      r$input$run_pred_class <- paste(input$run_pred_class)
      r$input$admit_date <- input$admit_date
      r$input$discharge_date <- input$discharge_date
      r$input$admit_type <- input$admit_type
      r$input$sex <- input$sex
      r$input$age <- input$age
      r$input$stay <- input$stay
      r$input$shock <- input$shock
      r$input$card_shock <- input$card_shock
      r$input$tlc <- input$tlc
      r$input$urea <- input$urea
      r$input$bnp <- input$bnp
      r$input$platelets <- input$platelets
      r$input$creatinine <- input$creatinine
      r$input$hb <- input$hb
      r$input$ef <- input$ef
    })


    ## Browser Action Button ----
    # Conditional action button implemented for developer debugging session
    # Using SHINY_PORT checks for whether session is local or on RSConnect
    if (Sys.getenv("SHINY_PORT") == "") {
      observeEvent(
        {
          input$browser
        },
        {
          browser()
        }
      )
    }
  })
}

## To be copied in the UI
# mod_class_sidebar_ui("class_sidebar_1")

## To be copied in the server
# mod_class_sidebar_server("class_sidebar_1")
