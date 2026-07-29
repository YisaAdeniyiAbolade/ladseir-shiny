data_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "app-page",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Data source",
        shiny::radioButtons(
          ns("data_source"), NULL,
          choices = c("Bundled RAPIDD scenario" = "bundled", "Upload CSV" = "upload"),
          selected = "bundled"
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'bundled'", ns("data_source")),
          shiny::selectInput(ns("scenario"), "RAPIDD scenario", choices = paste("Scenario", 1:4), selected = "Scenario 1")
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'upload'", ns("data_source")),
          shiny::fileInput(ns("file"), "CSV file", accept = c(".csv", "text/csv")),
          shiny::uiOutput(ns("mapping_ui")),
          shiny::numericInput(ns("uploaded_interval"), "Default interval length (days)", value = 7, min = 0.01, step = 1),
          shiny::actionButton(ns("apply_upload"), "Use uploaded data", class = "btn-primary w-100", icon = app_icon("upload")),
          htmltools::div(class = "mt-2", shiny::downloadButton(ns("template"), "Download CSV template", class = "btn-outline-secondary w-100"))
        ),
        htmltools::hr(),
        shiny::sliderInput(ns("calibration_end"), "Calibration endpoint", min = 8, max = 46, value = 20, step = 1),
        htmltools::p(class = "help-text", "The endpoint determines the observations supplied to calibration and paired comparison modules."),
        width = 330
      ),
      shiny::uiOutput(ns("error")),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        bslib::value_box(title = "Intervals", value = shiny::textOutput(ns("n_intervals"), inline = TRUE), showcase = app_icon("calendar3"), theme = "primary"),
        bslib::value_box(title = "Calibration intervals", value = shiny::textOutput(ns("n_calibration"), inline = TRUE), showcase = app_icon("sliders"), theme = "info"),
        bslib::value_box(title = "Total cases", value = shiny::textOutput(ns("total_cases"), inline = TRUE), showcase = app_icon("people"), theme = "success"),
        bslib::value_box(title = "Peak incidence", value = shiny::textOutput(ns("peak_cases"), inline = TRUE), showcase = app_icon("bar-chart-line"), theme = "warning")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Incidence series"),
        bslib::card_body(plotly::plotlyOutput(ns("incidence_plot"), height = "420px"))
      ),
      bslib::card(
        bslib::card_header("Analysis data", shiny::downloadButton(ns("download_data"), "Download", class = "btn-sm btn-outline-primary float-end")),
        bslib::card_body(DT::DTOutput(ns("data_table")))
      )
    )
  )
}

data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- shiny::reactiveValues(
      data = ladseir::ladseir_example_data(1),
      source = "RAPIDD Scenario 1",
      error = NULL
    )

    uploaded_raw <- shiny::reactive({
      shiny::req(input$file)
      read_uploaded_csv(input$file$datapath)
    })

    output$mapping_ui <- shiny::renderUI({
      if (is.null(input$file)) {
        return(empty_state("Upload a CSV", "Column mapping controls appear after a file is selected.", "file-earmark-arrow-up"))
      }
      data <- tryCatch(uploaded_raw(), error = function(e) NULL)
      if (is.null(data)) return(NULL)
      columns <- names(data)
      case_guess <- guess_column(columns, c("case", "count", "incidence", "observed"))
      date_guess <- guess_column(columns, c("date", "week_start", "time_date"))
      time_guess <- guess_column(columns, c("interval_end", "day", "time", "week"))
      htmltools::tagList(
        shiny::selectInput(ns("cases_column"), "Case-count column", choices = columns, selected = case_guess),
        shiny::selectInput(ns("date_column"), "Date column", choices = c("None" = "__none__", columns), selected = if (grepl("date", tolower(date_guess))) date_guess else "__none__"),
        shiny::selectInput(ns("time_column"), "Reporting-time column", choices = c("Infer from date/order" = "__none__", columns), selected = if (grepl("time|day|week|interval", tolower(time_guess))) time_guess else "__none__")
      )
    })

    shiny::observeEvent(input$scenario, {
      number <- as.integer(sub("Scenario ", "", input$scenario))
      state$data <- ladseir::ladseir_example_data(number)
      state$source <- paste("RAPIDD Scenario", number)
      state$error <- NULL
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$apply_upload, {
      state$error <- NULL
      tryCatch({
        data <- normalize_uploaded_data(
          uploaded_raw(),
          cases_column = input$cases_column,
          date_column = input$date_column,
          time_column = input$time_column,
          interval_length = input$uploaded_interval
        )
        validate_analysis_data(data)
        state$data <- data
        state$source <- input$file$name
      }, error = function(e) {
        state$error <- friendly_error(e)
      })
    })

    shiny::observe({
      data <- state$data
      minimum <- min(8L, nrow(data))
      selected <- min(max(minimum, input$calibration_end %||% min(20L, nrow(data))), nrow(data))
      shiny::updateSliderInput(session, "calibration_end", min = minimum, max = nrow(data), value = selected)
    })

    output$error <- shiny::renderUI({
      if (is.null(state$error)) return(NULL)
      htmltools::div(class = "alert alert-danger", app_icon("exclamation-triangle"), state$error)
    })

    calibration_end <- shiny::reactive({
      min(as.integer(input$calibration_end %||% nrow(state$data)), nrow(state$data))
    })

    output$n_intervals <- shiny::renderText(format_count(nrow(state$data)))
    output$n_calibration <- shiny::renderText(format_count(calibration_end()))
    output$total_cases <- shiny::renderText(format_count(sum(state$data$cases)))
    output$peak_cases <- shiny::renderText(format_count(max(state$data$cases)))

    output$incidence_plot <- plotly::renderPlotly({
      as_interactive_plot(plot_incidence_data(state$data, calibration_end()), tooltip = c("x", "y"))
    })

    output$data_table <- DT::renderDT({
      display <- state$data
      if (all(is.na(display$date))) display$date <- NULL
      DT::datatable(
        display,
        rownames = FALSE,
        options = list(pageLength = 12, scrollX = TRUE, dom = "tip"),
        class = "compact stripe hover"
      )
    })

    output$download_data <- shiny::downloadHandler(
      filename = function() paste0(safe_filename(state$source), "-analysis-data.csv"),
      content = function(file) utils::write.csv(state$data, file, row.names = FALSE)
    )

    output$template <- shiny::downloadHandler(
      filename = "ladseir-data-template.csv",
      content = write_data_template
    )

    list(
      data = shiny::reactive(state$data),
      training_data = shiny::reactive(state$data[seq_len(calibration_end()), , drop = FALSE]),
      metadata = shiny::reactive(list(source = state$source, calibration_end = calibration_end()))
    )
  })
}
