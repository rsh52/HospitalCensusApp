#' regression_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regression_sidebar_ui <- function(id){
  ns <- NS(id)
    sidebar(
      title = "Input Parameters",
      dateInput(ns("pred_date"), "Prediction Date", Sys.Date()),
      actionButton(ns("run_pred_regression"), label = "Run Prediction",
                   lib = "fontawesome", icon = icon("lightbulb")),
      # Conditional action button implemented for developer debugging session
      # Using SHINY_PORT checks for whether session is local or on RSConnect
      if (Sys.getenv("SHINY_PORT") == "") {
        actionButton(ns("browser"), label = "Initiate browser()")
      }
    )
}

#' regression_sidebar Server Functions
#'
#' @noRd
mod_regression_sidebar_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$run_pred_regression, {
      r$input$run_pred_regression <- paste(input$run_pred_regression)
      r$input$pred_date <- input$pred_date
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
# mod_regression_sidebar_ui("regression_sidebar_1")

## To be copied in the server
# mod_regression_sidebar_server("regression_sidebar_1")
