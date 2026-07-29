calibration_controls <- function(ns, include_loss = TRUE, button_id = "run", button_label = "Run calibration") {
  htmltools::tagList(
    shiny::selectInput(ns("driver"), "Transmission family", choices = stats::setNames(names(DRIVER_LABELS), DRIVER_LABELS), selected = "logistic_decline"),
    if (include_loss) shiny::selectInput(ns("loss"), "Calibration criterion", choices = stats::setNames(names(LOSS_LABELS), LOSS_LABELS), selected = "LAD"),
    shiny::numericInput(ns("population"), "Population", value = DEFAULT_POPULATION, min = 100, step = 1000),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Computation",
        shiny::numericInput(ns("starts"), "Multistart values", value = 5, min = 1, max = 50, step = 1),
        shiny::numericInput(ns("iterations"), "Maximum iterations", value = 1500, min = 100, max = 10000, step = 100),
        shiny::numericInput(ns("seed"), "Random seed", value = DEFAULT_SEED, min = 1, step = 1)
      ),
      bslib::accordion_panel(
        "Fixed epidemiological rates",
        shiny::checkboxInput(ns("fix_sigma"), "Fix progression rate σ", value = FALSE),
        shiny::numericInput(ns("sigma"), "σ", value = 1 / 11.4, min = 1 / 21, max = 1 / 3, step = 0.001),
        shiny::checkboxInput(ns("fix_gamma"), "Fix removal rate γ", value = FALSE),
        shiny::numericInput(ns("gamma"), "γ", value = 1 / 7, min = 1 / 21, max = 1 / 2, step = 0.001)
      )
    ),
    shiny::actionButton(ns(button_id), button_label, class = "btn-primary w-100", icon = app_icon("play-fill"))
  )
}

calibration_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "app-page",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Calibration settings",
        calibration_controls(ns),
        htmltools::p(class = "help-text mt-3", "Use the Data page to select the series and calibration endpoint."),
        width = 340
      ),
      shiny::uiOutput(ns("status_message")),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        bslib::value_box(title = "Status", value = shiny::uiOutput(ns("convergence"), inline = TRUE), showcase = app_icon("check-circle"), theme = "primary"),
        bslib::value_box(title = "Objective", value = shiny::textOutput(ns("objective"), inline = TRUE), showcase = app_icon("bullseye"), theme = "info"),
        bslib::value_box(title = "MAE", value = shiny::textOutput(ns("mae"), inline = TRUE), showcase = app_icon("arrows-collapse"), theme = "success"),
        bslib::value_box(title = "RMSE", value = shiny::textOutput(ns("rmse"), inline = TRUE), showcase = app_icon("activity"), theme = "warning")
      ),
      bslib::navset_card_tab(
        id = ns("result_tabs"),
        full_screen = TRUE,
        bslib::nav_panel("Observed and fitted", plotly::plotlyOutput(ns("fit_plot"), height = "480px")),
        bslib::nav_panel("Residuals", plotly::plotlyOutput(ns("residual_plot"), height = "480px")),
        bslib::nav_panel("Transmission", plotly::plotlyOutput(ns("transmission_plot"), height = "480px")),
        bslib::nav_panel("Parameters", DT::DTOutput(ns("parameter_table"))),
        bslib::nav_panel("Optimization", DT::DTOutput(ns("optimization_table")))
      ),
      bslib::card(
        bslib::card_header("Export analysis"),
        bslib::card_body(
          htmltools::div(
            class = "download-row",
            shiny::downloadButton(ns("download_fit"), "Fitted series", class = "btn-outline-primary"),
            shiny::downloadButton(ns("download_parameters"), "Parameters", class = "btn-outline-primary"),
            shiny::downloadButton(ns("download_report"), "HTML report", class = "btn-outline-primary")
          )
        )
      )
    )
  )
}

calibration_server <- function(id, training_data, metadata) {
  shiny::moduleServer(id, function(input, output, session) {
    fit_value <- shiny::reactiveVal(NULL)
    settings_value <- shiny::reactiveVal(NULL)
    error_value <- shiny::reactiveVal(NULL)

    shiny::observeEvent(training_data(), {
      fit_value(NULL)
      settings_value(NULL)
      error_value(NULL)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      error_value(NULL)
      fit_value(NULL)
      data <- training_data()
      tryCatch({
        validate_analysis_data(data)
        settings <- make_settings(input$population, input$starts, input$iterations)
        fixed <- fixed_parameter_vector(input$fix_sigma, input$sigma, input$fix_gamma, input$gamma)
        fit <- shiny::withProgress(message = "Calibrating the SEIR model", value = 0.15, {
          shiny::incProgress(0.25, detail = "Generating feasible starting values")
          result <- ladseir::fit_lad_seir(
            observed = data$cases,
            interval_end_times = data$interval_end_time,
            driver = input$driver,
            loss = input$loss,
            settings = settings,
            fixed_parameters = fixed,
            seed = as.integer(input$seed)
          )
          shiny::incProgress(0.60, detail = "Preparing diagnostics")
          result
        })
        settings_value(settings)
        fit_value(fit)
      }, error = function(e) {
        error_value(friendly_error(e))
      })
    })

    output$status_message <- shiny::renderUI({
      if (!is.null(error_value())) return(htmltools::div(class = "alert alert-danger", app_icon("exclamation-triangle"), error_value()))
      if (is.null(fit_value())) return(empty_state("Calibration ready", "Configure the model and select Run calibration.", "sliders2"))
      fit <- fit_value()
      htmltools::div(
        class = if (fit$convergence == 0L) "alert alert-success" else "alert alert-warning",
        app_icon(if (fit$convergence == 0L) "check-circle" else "exclamation-circle"),
        if (fit$convergence == 0L) "Calibration completed successfully." else paste("Calibration completed with convergence code", fit$convergence, ".")
      )
    })

    output$convergence <- shiny::renderUI({
      fit <- fit_value()
      if (is.null(fit)) return("-")
      status_badge(fit$convergence)
    })
    output$objective <- shiny::renderText(if (is.null(fit_value())) "-" else format_metric(fit_value()$objective, 6))
    output$mae <- shiny::renderText({
      fit <- fit_value(); if (is.null(fit)) return("-")
      format_metric(calculate_metrics(fit$observations, fit$fitted)["MAE"], 5)
    })
    output$rmse <- shiny::renderText({
      fit <- fit_value(); if (is.null(fit)) return("-")
      format_metric(calculate_metrics(fit$observations, fit$fitted)["RMSE"], 5)
    })

    output$fit_plot <- plotly::renderPlotly({
      shiny::req(fit_value())
      as_interactive_plot(plot_fit_result(fit_value()), tooltip = c("x", "y", "colour"))
    })
    output$residual_plot <- plotly::renderPlotly({
      shiny::req(fit_value())
      as_interactive_plot(plot_residual_result(fit_value()), tooltip = c("x", "y"))
    })
    output$transmission_plot <- plotly::renderPlotly({
      shiny::req(fit_value())
      as_interactive_plot(plot_transmission_result(fit_value()), tooltip = c("x", "y"))
    })

    output$parameter_table <- DT::renderDT({
      shiny::req(fit_value())
      table <- parameter_frame(fit_value())
      table$estimate <- signif(table$estimate, 8)
      DT::datatable(table, rownames = FALSE, options = list(dom = "t", pageLength = 20), class = "compact stripe")
    })

    output$optimization_table <- DT::renderDT({
      shiny::req(fit_value())
      table <- fit_value()$optimization_runs
      table$objective <- signif(table$objective, 8)
      DT::datatable(table, rownames = FALSE, options = list(dom = "tip", pageLength = 10), class = "compact stripe")
    })

    output$download_fit <- shiny::downloadHandler(
      filename = function() "ladseir-fitted-series.csv",
      content = function(file) { shiny::req(fit_value()); utils::write.csv(fit_frame(fit_value()), file, row.names = FALSE) }
    )
    output$download_parameters <- shiny::downloadHandler(
      filename = function() "ladseir-parameter-estimates.csv",
      content = function(file) { shiny::req(fit_value()); utils::write.csv(parameter_frame(fit_value()), file, row.names = FALSE) }
    )
    output$download_report <- shiny::downloadHandler(
      filename = function() "ladseir-calibration-report.html",
      content = function(file) {
        shiny::req(fit_value())
        write_fit_html(
          fit_value(), file,
          metadata = c(metadata(), list(app_version = APP_VERSION, package_version = as.character(utils::packageVersion("ladseir"))))
        )
      }
    )

    list(
      fit = shiny::reactive(fit_value()),
      settings = shiny::reactive(settings_value())
    )
  })
}
