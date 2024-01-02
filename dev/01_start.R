#
#
#
golem::fill_desc(
  pkg_name = "HospitalCensusApp",
  pkg_title = "Transplant Census Prediction Application",
  pkg_description = "A shiny application for prediction of transplant census based on user inputs.",
  author_first_name = "Ezra",
  author_last_name = "Porter",
  author_email = "porterej@chop.edu",
  repo_url = NULL,
  pkg_version = "0.0.0.9000"
)
golem::set_golem_options()
golem::install_dev_deps()
usethis::use_mit_license("Golem User")
usethis::use_readme_rmd(open = FALSE)
devtools::build_readme()
usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)
usethis::use_git()
golem::use_recommended_tests()
golem::use_favicon()
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)
rstudioapi::navigateToFile("dev/02_dev.R")
