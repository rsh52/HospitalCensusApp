#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    waiter_preloader(html = waiter_html(), color = "white"),
    page_navbar(
      nav_panel("Patient Classification Modeling", mod_class_ui("class_1")),
      nav_panel("Census Regression Modeling", mod_regression_ui("regression_1"))
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "HospitalCensusApp"
    ),
    includeCSS("https://github.research.chop.edu/pages/CQI/chop-bootstrap/bootstrap-5/bootstrap.min.css"),
    useWaiter()
  )
}
