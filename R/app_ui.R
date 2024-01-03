#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # bs_theme_preview(),
    golem_add_external_resources(),
    waiter_preloader(html = waiter_html(), color = "white"),
    page_navbar(
      title = "Hospital Census Application",
      theme = bs_theme_update(bslib::bs_theme(),
                              fg = "#000",
                              primary = "#008cba",
                              secondary = "#DBDDDF",
                              font_scale = NULL,
                              `enable-shadows` = TRUE,
                              preset = "litera",
                              bg = "#fff"),
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
    # includeCSS("inst/app/www/custom.css"),
    useWaiter()
  )
}
