options(warn = 1)

if (!file.exists("app.R") || !file.exists("DESCRIPTION")) {
  stop("Run this script from the ladseir-shiny repository root.", call. = FALSE)
}

required <- c("shiny", "bslib", "bsicons", "ggplot2", "plotly", "DT", "jsonlite", "htmltools", "testthat", "ladseir")
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) stop("Missing dependencies: ", paste(missing, collapse = ", "), call. = FALSE)

message("1. Checking repository structure...")
required_files <- c(
  "app.R", "DESCRIPTION", "README.md", "LICENSE", "www/styles.css", "www/logo.svg",
  "R/00_config.R", "R/11_module_data.R", "R/12_module_calibration.R",
  "R/13_module_comparison.R", "R/14_module_forecast.R", "R/15_module_simulation.R"
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) stop("Missing application files: ", paste(missing_files, collapse = ", "), call. = FALSE)

message("2. Running unit tests...")
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)

message("3. Constructing the Shiny application...")
environment <- new.env(parent = globalenv())
result <- source("app.R", local = environment)$value
if (!inherits(result, "shiny.appobj")) stop("app.R did not return a Shiny application object.", call. = FALSE)
rendered <- htmltools::renderTags(environment$ui)
if (!nzchar(rendered$html)) stop("The application UI rendered as empty HTML.", call. = FALSE)

message("4. Running a computational smoke test...")
data <- ladseir::ladseir_example_data(1)[1:12, ]
settings <- ladseir::ladseir_settings(
  population = 4499621,
  starting_values = 1,
  maximum_iterations = 400
)
fit <- ladseir::fit_lad_seir(
  observed = data$cases,
  interval_end_times = data$interval_end_time,
  driver = "logistic_decline",
  loss = "LAD",
  settings = settings,
  fixed_parameters = c(sigma = 1 / 11.4, gamma = 1 / 7),
  seed = 20260627
)
if (!inherits(fit, "ladseir_fit")) stop("The calibration smoke test did not return a ladseir_fit.", call. = FALSE)
forecast <- ladseir::forecast_lad_seir(fit, horizon = 2, draws = 100, seed = 20260627)
if (nrow(forecast) != 2L || any(!is.finite(forecast$mean))) stop("The forecast smoke test failed.", call. = FALSE)

message("Application validation completed successfully.")
message("App version: ", environment$APP_VERSION)
message("ladseir version: ", as.character(utils::packageVersion("ladseir")))
