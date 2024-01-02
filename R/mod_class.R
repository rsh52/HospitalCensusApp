#' class UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_class_ui <- function(id){
  ns <- NS(id)
  tagList(
    page_sidebar(
      sidebar = mod_class_sidebar_ui("class_sidebar_1"),
      layout_columns(
        value_box(
          title = "Model Data Sample Size",
          value = textOutput(ns("ha_size")),
          showcase = bs_icon("person"), fill = FALSE,
          theme_color = "primary",
          height = "10vh"
        ),
        value_box(
          title = "Model Performance",
          value = textOutput(ns("model_perf")),
          showcase = bs_icon("clipboard-data"), fill = FALSE,
          theme_color = "primary",
          height = "10vh"
        )
      ),
      layout_columns(
        uiOutput(ns("class_prediction")),
        uiOutput(ns("class_prediction_prob"))
      ),
      card(
        card_header("ROC By Outcome Class"),
        plotOutput(ns("roc_plot")),
        height = "40vh", full_screen = TRUE
      )
    )
  )
}

#' class Server Functions
#'
#' @noRd
mod_class_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ha_size <- renderText({
      r$ha_data |>
        nrow()
    })

    output$model_perf <- renderText({
      roc_auc_val <- r$roc_auc_value_vip |>
        pull(.estimate)

      round(roc_auc_val, 4)
    })

    output$roc_plot <- renderPlot({
      r$roc_plot_vip
    })

    # Placeholder box until a prediction is run
    output$class_prediction <- renderUI({value_box(
      title = "Outcome Probability:",
      value = "Awaiting prediction parameters",
      showcase = bs_icon("hourglass"),
      theme = "primary",
      height = "20vh"
    )})

    observeEvent(r$input$run_pred_class, {
      output$class_prediction <- renderUI({
        r$class_prediction <- predict(
          r$class_model,
          tibble(
            mrn = factor(12345), #temp
            age = r$input$age,
            sex = factor(r$input$sex),
            admit_type = factor(r$input$admit_type),
            date_arrival = r$input$admit_date,
            date_discharge = r$input$discharge_date,
            n_days = as.numeric(difftime(r$input$admit_date, r$input$discharge_date, units = "days")),
            `duration of stay` = r$input$stay,
            `hb` = r$input$hb,
            `tlc` = r$input$tlc,
            `platelets` = r$input$platelets,
            `urea` = r$input$urea,
            `creatinine` = r$input$creatinine,
            `bnp` = r$input$bnp,
            `ef` = r$input$ef,
            `cardiogenic shock` = as.double(r$input$card_shock),
            `shock` = as.double(r$input$shock)
          )
        ) |>
          pull(.pred_class) |>
          as.character() |>
          str_to_title()

        value_box(
          title = "Outcome Prediction:",
          value = HTML(r$class_prediction),
          showcase = bs_icon("hospital"), fill = FALSE,
          theme = "primary",
          height = "20vh"
        )
      })
    })

    # Placeholder box until a prediction is run
    output$class_prediction_prob <- renderUI({value_box(
      title = "Outcome Probability:",
      value = "Awaiting prediction parameters",
      showcase = bs_icon("hourglass"),
      height = "20vh",
      theme = "primary"
    )})

    observeEvent(r$input$run_pred_class, {
      output$class_prediction_prob <- renderUI({
        r$class_prediction_prob <- predict(
          r$class_model,
          tibble(
            mrn = factor(12345), #temp
            age = r$input$age,
            sex = factor(r$input$sex),
            admit_type = factor(r$input$admit_type),
            date_arrival = r$input$admit_date,
            date_discharge = r$input$discharge_date,
            n_days = as.numeric(difftime(r$input$admit_date, r$input$discharge_date, units = "days")),
            `duration of stay` = r$input$stay,
            `hb` = r$input$hb,
            `tlc` = r$input$tlc,
            `platelets` = r$input$platelets,
            `urea` = r$input$urea,
            `creatinine` = r$input$creatinine,
            `bnp` = r$input$bnp,
            `ef` = r$input$ef,
            `cardiogenic shock` = as.double(r$input$card_shock),
            `shock` = as.double(r$input$shock)
          ), type = "prob"
        )

        out <- r$class_prediction_prob * 100
        out <- round(out, 2)
        value_string <- out |>
          rename_all(~ str_remove(., ".pred_")) %>%
          pivot_longer(cols = everything()) %>%
          arrange(desc(value)) |>
          reframe(
            text = paste0("<h5><b>", name, "</b>: ", value, "</h5>")
          ) %>%
          pull(text) %>%
          paste(collapse = "")

        value_box(
          title = "Outcome Probability:",
          value = HTML(value_string),
          showcase = bs_icon("percent"),
          theme = "primary",
          height = "20vh"
        )
      })
    })

  })
}

## To be copied in the UI
# mod_class_ui("class_1")

## To be copied in the server
# mod_class_server("class_1")
