if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install rsconnect before deployment: install.packages('rsconnect').", call. = FALSE)
}
if (!file.exists("app.R")) stop("Run this script from the ladseir-shiny repository root.", call. = FALSE)
if (!requireNamespace("ladseir", quietly = TRUE)) {
  stop("Install ladseir from GitHub before deployment.", call. = FALSE)
}

rsconnect::deployApp(
  appDir = ".",
  appName = "ladseir-laboratory",
  appTitle = "LAD-SEIR Interactive Calibration and Forecasting Laboratory",
  forceUpdate = TRUE,
  launch.browser = TRUE
)
