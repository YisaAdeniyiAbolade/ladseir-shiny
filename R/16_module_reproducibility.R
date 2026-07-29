reproducibility_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "app-page",
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::value_box(title = "Application version", value = APP_VERSION, showcase = app_icon("window"), theme = "primary"),
      bslib::value_box(title = "ladseir version", value = shiny::textOutput(ns("package_version"), inline = TRUE), showcase = app_icon("box-seam"), theme = "info"),
      bslib::value_box(title = "R version", value = shiny::textOutput(ns("r_version"), inline = TRUE), showcase = app_icon("terminal"), theme = "success")
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Reproducibility record"),
        bslib::card_body(
          htmltools::p("Every analysis is determined by the selected dataset, calibration endpoint, model configuration, optimizer settings, and random seed."),
          htmltools::p("Downloaded fitted series, parameter tables, forecasts, and HTML reports preserve the results needed for independent review."),
          htmltools::div(
            class = "download-row",
            shiny::downloadButton(ns("download_record"), "Download run record", class = "btn-primary"),
            htmltools::a(class = "btn btn-outline-primary", href = CORE_REPOSITORY, target = "_blank", app_icon("github"), " Core repository")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Data handling"),
        bslib::card_body(
          htmltools::p("Uploaded CSV files are processed within the active Shiny session. The application code does not write uploaded data to persistent storage."),
          htmltools::p("Do not upload confidential, personally identifiable, or regulated health information to a public deployment."),
          htmltools::p(class = "mb-0", htmltools::tags$b("Recommended input:"), " aggregated reporting-interval incidence.")
        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Session information"),
      bslib::card_body(htmltools::pre(class = "session-info", shiny::textOutput(ns("session_info"))))
    ),
    bslib::card(
      bslib::card_header("Software architecture"),
      bslib::card_body(
        htmltools::div(
          class = "architecture-grid",
          htmltools::div(class = "architecture-node", app_icon("database"), htmltools::h4("Data"), htmltools::p("Bundled RAPIDD scenarios or user-supplied reporting-interval incidence.")),
          htmltools::div(class = "architecture-arrow", app_icon("arrow-right")),
          htmltools::div(class = "architecture-node", app_icon("box-seam"), htmltools::h4("ladseir"), htmltools::p("Validated statistical engine for calibration, comparison, simulation, and forecasting.")),
          htmltools::div(class = "architecture-arrow", app_icon("arrow-right")),
          htmltools::div(class = "architecture-node", app_icon("window"), htmltools::h4("Shiny interface"), htmltools::p("Interactive controls, diagnostics, visualizations, and reproducible downloads."))
        )
      )
    )
  )
}

reproducibility_server <- function(id, data_state, fit, comparison, forecast) {
  shiny::moduleServer(id, function(input, output, session) {
    output$package_version <- shiny::renderText(as.character(utils::packageVersion("ladseir")))
    output$r_version <- shiny::renderText(paste(R.version$major, sub(" .*", "", R.version$minor), sep = "."))
    output$session_info <- shiny::renderText(paste(capture.output(utils::sessionInfo()), collapse = "\n"))

    output$download_record <- shiny::downloadHandler(
      filename = function() paste0("ladseir-run-record-", format(Sys.Date(), "%Y%m%d"), ".json"),
      content = function(file) {
        current_fit <- fit()
        current_comparison <- comparison()
        current_forecast <- forecast()
        record <- list(
          generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
          application = list(name = "LAD-SEIR Interactive Laboratory", version = APP_VERSION, repository = APP_REPOSITORY),
          core_package = list(version = as.character(utils::packageVersion("ladseir")), repository = CORE_REPOSITORY),
          data = list(rows = nrow(data_state()), total_cases = sum(data_state()$cases), final_time = max(data_state()$interval_end_time)),
          calibration = if (is.null(current_fit)) NULL else list(
            driver = current_fit$driver,
            loss = current_fit$loss,
            parameters = as.list(current_fit$parameters),
            objective = current_fit$objective,
            convergence = current_fit$convergence,
            metrics = as.list(calculate_metrics(current_fit$observations, current_fit$fitted))
          ),
          comparison = if (is.null(current_comparison)) NULL else comparison_metrics_frame(current_comparison),
          forecast = current_forecast,
          session = capture.output(utils::sessionInfo())
        )
        jsonlite::write_json(record, file, pretty = TRUE, auto_unbox = TRUE, digits = 10, null = "null")
      }
    )
  })
}
