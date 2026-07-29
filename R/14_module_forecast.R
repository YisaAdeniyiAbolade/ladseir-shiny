forecast_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "app-page",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Forecast settings",
        shiny::numericInput(ns("horizon"), "Forecast horizon (intervals)", value = 5, min = 1, max = 30, step = 1),
        shiny::numericInput(ns("interval_length"), "Interval length (days)", value = DEFAULT_INTERVAL_LENGTH, min = 0.01, step = 1),
        shiny::sliderInput(ns("level"), "Predictive interval", min = 0.50, max = 0.99, value = 0.95, step = 0.01),
        shiny::selectInput(ns("method"), "Predictive method", choices = stats::setNames(names(PREDICTIVE_METHOD_LABELS), PREDICTIVE_METHOD_LABELS), selected = "negative_binomial"),
        shiny::numericInput(ns("draws"), "Predictive draws", value = 1000, min = 100, max = 10000, step = 100),
        shiny::numericInput(ns("seed"), "Random seed", value = DEFAULT_SEED, min = 1, step = 1),
        shiny::actionButton(ns("run"), "Generate forecast", class = "btn-primary w-100", icon = app_icon("graph-up-arrow")),
        htmltools::p(class = "help-text mt-3", "Forecasts use the most recent successful model from the Calibration page."),
        width = 330
      ),
      shiny::uiOutput(ns("status_message")),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        bslib::value_box(title = "Model", value = shiny::textOutput(ns("model_label"), inline = TRUE), showcase = app_icon("diagram-3"), theme = "primary"),
        bslib::value_box(title = "Horizon", value = shiny::textOutput(ns("horizon_value"), inline = TRUE), showcase = app_icon("calendar-range"), theme = "info"),
        bslib::value_box(title = "First median", value = shiny::textOutput(ns("first_median"), inline = TRUE), showcase = app_icon("bar-chart"), theme = "success"),
        bslib::value_box(title = "Interval level", value = shiny::textOutput(ns("level_value"), inline = TRUE), showcase = app_icon("arrows-expand"), theme = "warning")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Forecast trajectory"),
        bslib::card_body(plotly::plotlyOutput(ns("forecast_plot"), height = "500px"))
      ),
      bslib::card(
        bslib::card_header("Forecast table", shiny::downloadButton(ns("download"), "Download", class = "btn-sm btn-outline-primary float-end")),
        bslib::card_body(DT::DTOutput(ns("forecast_table")))
      )
    )
  )
}

forecast_server <- function(id, fit) {
  shiny::moduleServer(id, function(input, output, session) {
    forecast_value <- shiny::reactiveVal(NULL)
    forecast_fit <- shiny::reactiveVal(NULL)
    error_value <- shiny::reactiveVal(NULL)

    shiny::observeEvent(fit(), {
      forecast_value(NULL)
      forecast_fit(NULL)
      error_value(NULL)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$run, {
      forecast_value(NULL)
      error_value(NULL)
      current_fit <- fit()
      if (is.null(current_fit)) {
        error_value("Run a successful calibration before generating a forecast.")
        return()
      }
      tryCatch({
        result <- shiny::withProgress(message = "Generating predictive forecast", value = 0.25, {
          forecast <- ladseir::forecast_lad_seir(
            current_fit,
            horizon = as.integer(input$horizon),
            interval_length = as.numeric(input$interval_length),
            interval = TRUE,
            level = as.numeric(input$level),
            draws = as.integer(input$draws),
            method = input$method,
            seed = as.integer(input$seed)
          )
          shiny::incProgress(0.65, detail = "Summarizing predictive draws")
          forecast
        })
        forecast_fit(current_fit)
        forecast_value(result)
      }, error = function(e) error_value(friendly_error(e)))
    })

    output$status_message <- shiny::renderUI({
      if (!is.null(error_value())) return(htmltools::div(class = "alert alert-danger", app_icon("exclamation-triangle"), error_value()))
      if (is.null(fit())) return(empty_state("Calibration required", "Complete a model calibration before opening the forecast workflow.", "lock"))
      if (is.null(forecast_value())) return(empty_state("Forecast ready", "The current calibrated model is available. Select settings and generate the forecast.", "graph-up"))
      htmltools::div(class = "alert alert-success", app_icon("check-circle"), "Forecast and predictive intervals generated successfully.")
    })

    output$model_label <- shiny::renderText({
      current <- forecast_fit() %||% fit()
      if (is.null(current)) return("-")
      paste(current$loss, "·", DRIVER_LABELS[[current$driver]])
    })
    output$horizon_value <- shiny::renderText(if (is.null(forecast_value())) "-" else paste(nrow(forecast_value()), "intervals"))
    output$first_median <- shiny::renderText(if (is.null(forecast_value())) "-" else format_count(forecast_value()$median[1]))
    output$level_value <- shiny::renderText(if (is.null(forecast_value())) "-" else paste0(round(input$level * 100), "%"))

    output$forecast_plot <- plotly::renderPlotly({
      shiny::req(forecast_value(), forecast_fit())
      as_interactive_plot(plot_forecast_result(forecast_fit(), forecast_value()), tooltip = c("x", "y", "colour", "ymin", "ymax"))
    })

    output$forecast_table <- DT::renderDT({
      shiny::req(forecast_value())
      table <- forecast_value()
      numeric <- vapply(table, is.numeric, logical(1))
      table[numeric] <- lapply(table[numeric], function(x) round(x, 4))
      DT::datatable(table, rownames = FALSE, options = list(dom = "t", scrollX = TRUE), class = "compact stripe")
    })

    output$download <- shiny::downloadHandler(
      filename = function() "ladseir-forecast.csv",
      content = function(file) { shiny::req(forecast_value()); utils::write.csv(forecast_value(), file, row.names = FALSE) }
    )

    list(forecast = shiny::reactive(forecast_value()))
  })
}
