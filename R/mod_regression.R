#' regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regression_ui <- function(id){
  ns <- NS(id)
  tagList(
    page_sidebar(
      sidebar = mod_regression_sidebar_ui("regression_sidebar_1"),
      layout_columns(
        uiOutput(ns("linreg_prediction")),
        uiOutput(ns("arima_prediction"))
      ),
      card(
        card_header("Linear Model Trendline"),
        plotOutput(ns("regression_trendline"))
      )
    )
  )
}

#' regression Server Functions
#'
#' @noRd
mod_regression_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Linear regression Prediction ----
    # Placeholder box until a prediction is run
    output$linreg_prediction <- renderUI({value_box(
      title = "Linear Regression Census Prediction:",
      value = "Awaiting prediction parameters",
      showcase = bs_icon("hourglass"),
      theme = "primary",
      height = "20vh"
    )})

    observeEvent(r$input$run_pred_regression, {
      output$linreg_prediction <- renderUI({
        r$linreg_prediction <- predict(
          r$linreg_model,
          tibble(
            date = lubridate::as_date(r$input$pred_date)
          )
        ) |>
          pull(.pred) |>
          round(1)

        value_box(
          title = "Linear Regression Census Prediction:",
          value = HTML(r$linreg_prediction),
          showcase = bs_icon("hospital"), fill = FALSE,
          theme = "primary",
          height = "20vh"
        )
      })
    })

    # ARIMA regression Prediction ----
    # Placeholder box until a prediction is run
    output$arima_prediction <- renderUI({value_box(
      title = "ARIMA Regression Census Prediction:",
      value = "Awaiting prediction parameters",
      showcase = bs_icon("hourglass"),
      theme = "primary",
      height = "20vh"
    )})

    observeEvent(r$input$run_pred_regression, {
      output$arima_prediction <- renderUI({
        r$arima_prediction <- predict(
          r$arima_model,
          tibble(
            date = lubridate::as_date(r$input$pred_date)
          )
        ) |>
          pull(.pred) |>
          round(1)

        value_box(
          title = "ARIMA Regression Census Prediction:",
          value = HTML(r$arima_prediction),
          showcase = bs_icon("hospital"), fill = FALSE,
          theme = "primary",
          height = "20vh"
        )
      })
    })

    # Regression Trendlines ----
    output$regression_trendline <- renderPlot({
      linreg_preds <- predict(r$linreg_model, r$ha_long) |>
        rename("Linear Regression Predicted" = .pred)
      arima_preds <- predict(r$arima_model, r$ha_long) |>
        rename("ARIMA Predicted" = .pred)

      regression_df <- cbind(r$ha_long, linreg_preds) |>
        cbind(arima_preds) |>
        tibble() |>
        dplyr::rename(
          "True Census" = count,
          "Date" = date
        ) |>
        pivot_longer(
          cols = c(`True Census`, `Linear Regression Predicted`, `ARIMA Predicted`),
          values_to = "Census",
          names_to = "Data Type"
        )

      ggplot(regression_df, aes(x = Date, y = Census, color = `Data Type`)) +
        geom_line() +
        theme_minimal()

    })

  })
}

## To be copied in the UI
# mod_regression_ui("regression_1")

## To be copied in the server
# mod_regression_server("regression_1")
