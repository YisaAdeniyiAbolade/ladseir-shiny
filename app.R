options(shiny.maxRequestSize = 10 * 1024^2)

required_packages <- c("shiny", "bslib", "bsicons", "ggplot2", "plotly", "DT", "jsonlite", "htmltools", "ladseir")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_packages)) {
  stop(
    "Install the required packages before running the application: ",
    paste(missing_packages, collapse = ", "),
    ". Run Rscript scripts/install_dependencies.R.",
    call. = FALSE
  )
}

if (utils::packageVersion("ladseir") < "0.1.0") {
  stop("ladseir version 0.1.0 or later is required. Reinstall it from the core GitHub repository.", call. = FALSE)
}

invisible(lapply(sort(list.files("R", pattern = "\\.R$", full.names = TRUE)), source, local = FALSE))

ui <- htmltools::tagList(
  htmltools::tags$head(
    htmltools::tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    htmltools::includeCSS("www/styles.css")
  ),
  bslib::page_navbar(
  id = "main_nav",
  title = htmltools::div(
    class = "navbar-brand-wrap",
    htmltools::img(src = "logo.svg", alt = "LAD-SEIR", class = "navbar-logo"),
    htmltools::span("LAD-SEIR Laboratory")
  ),
  window_title = "LAD-SEIR Interactive Calibration and Forecasting Laboratory",
  theme = APP_THEME,
  fillable = FALSE,
  bslib::nav_panel("Overview", home_ui("home"), value = "Overview", icon = app_icon("house")),
  bslib::nav_panel("Data", data_ui("data"), value = "Data", icon = app_icon("database")),
  bslib::nav_panel("Calibration", calibration_ui("calibration"), value = "Calibration", icon = app_icon("sliders")),
  bslib::nav_panel("LAD vs LSQ", comparison_ui("comparison"), value = "LAD vs LSQ", icon = app_icon("columns-gap")),
  bslib::nav_panel("Forecast", forecast_ui("forecast"), value = "Forecast", icon = app_icon("graph-up-arrow")),
  bslib::nav_panel("Simulation", simulation_ui("simulation"), value = "Simulation", icon = app_icon("dice-5")),
  bslib::nav_panel("Reproducibility", reproducibility_ui("reproducibility"), value = "Reproducibility", icon = app_icon("journal-check")),
  bslib::nav_spacer(),
  bslib::nav_item(htmltools::a(href = CORE_REPOSITORY, target = "_blank", class = "nav-link external-nav", app_icon("github"), " R package"))
  )
)

server <- function(input, output, session) {
  home_server("home")
  data_state <- data_server("data")
  calibration_state <- calibration_server("calibration", data_state$training_data, data_state$metadata)
  comparison_state <- comparison_server("comparison", data_state$training_data)
  forecast_state <- forecast_server("forecast", calibration_state$fit)
  simulation_server("simulation")
  reproducibility_server(
    "reproducibility",
    data_state = data_state$data,
    fit = calibration_state$fit,
    comparison = comparison_state$comparison,
    forecast = forecast_state$forecast
  )
}

app <- shiny::shinyApp(ui = ui, server = server)
app
