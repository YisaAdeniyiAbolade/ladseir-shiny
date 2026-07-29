simulation_parameter_ui <- function(driver, ns) {
  common <- htmltools::tagList(
    shiny::numericInput(ns("p_beta0"), "Baseline transmission β₀", value = if (driver == "logistic_decline") 0.50 else 0.36, min = 0.0001, max = 3, step = 0.01),
    shiny::numericInput(ns("p_sigma"), "Progression rate σ", value = 1 / 7, min = 1 / 21, max = 1 / 3, step = 0.001),
    shiny::numericInput(ns("p_gamma"), "Removal rate γ", value = 1 / 6.5, min = 1 / 21, max = 1 / 2, step = 0.001)
  )
  driver_specific <- switch(
    driver,
    cosine = htmltools::tagList(
      shiny::numericInput(ns("p_a"), "Cosine amplitude a", value = 0.25, min = -0.95, max = 0.95, step = 0.05),
      shiny::numericInput(ns("p_omega"), "Angular frequency ω", value = 2 * pi / 120, min = 2 * pi / 365, max = 2 * pi / 21, step = 0.001)
    ),
    exponential = htmltools::tagList(
      shiny::numericInput(ns("p_a"), "Exponential amplitude a", value = 0.60, min = -0.95, max = 2, step = 0.05),
      shiny::numericInput(ns("p_b"), "Exponential rate b", value = -0.015, min = -0.05, max = 0.05, step = 0.001)
    ),
    logistic_decline = htmltools::tagList(
      shiny::numericInput(ns("p_q"), "Long-run fraction q", value = 0.25, min = 0.05, max = 0.95, step = 0.05),
      shiny::numericInput(ns("p_k"), "Decline rate k", value = 0.08, min = 0.005, max = 0.50, step = 0.005),
      shiny::numericInput(ns("p_tau"), "Midpoint τ (days)", value = 70, min = 7, max = 365, step = 1)
    )
  )
  htmltools::tagList(common, driver_specific,
    shiny::numericInput(ns("p_E0"), "Initial exposed E₀", value = 40, min = 0.000001, step = 1),
    shiny::numericInput(ns("p_I0"), "Initial infectious I₀", value = 20, min = 0.000001, step = 1)
  )
}

simulation_parameters <- function(input, driver) {
  common <- c(beta0 = input$p_beta0, sigma = input$p_sigma, gamma = input$p_gamma)
  specific <- switch(
    driver,
    cosine = c(a = input$p_a, omega = input$p_omega),
    exponential = c(a = input$p_a, b = input$p_b),
    logistic_decline = c(q = input$p_q, k = input$p_k, tau = input$p_tau)
  )
  c(common, specific, E0 = input$p_E0, I0 = input$p_I0)
}

simulation_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "app-page",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Simulation design",
        shiny::selectInput(ns("driver"), "Transmission family", choices = stats::setNames(names(DRIVER_LABELS), DRIVER_LABELS), selected = "logistic_decline"),
        shiny::numericInput(ns("population"), "Population", value = 1000000, min = 100, step = 1000),
        shiny::numericInput(ns("intervals"), "Reporting intervals", value = 30, min = 8, max = 120, step = 1),
        shiny::numericInput(ns("interval_length"), "Interval length (days)", value = 7, min = 0.01, step = 1),
        shiny::numericInput(ns("size"), "Negative-binomial size", value = 30, min = 0.1, step = 1),
        shiny::selectInput(ns("anomaly"), "Reporting mechanism", choices = stats::setNames(names(ANOMALY_LABELS), ANOMALY_LABELS), selected = "none"),
        shiny::numericInput(ns("seed"), "Random seed", value = DEFAULT_SEED, min = 1, step = 1),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel("Model parameters", shiny::uiOutput(ns("parameter_ui"))),
          bslib::accordion_panel(
            "Comparison computation",
            shiny::numericInput(ns("starts"), "Multistart values", value = 3, min = 1, max = 20, step = 1),
            shiny::numericInput(ns("iterations"), "Maximum iterations", value = 1000, min = 100, max = 5000, step = 100)
          )
        ),
        shiny::actionButton(ns("simulate"), "Simulate data", class = "btn-primary w-100", icon = app_icon("dice-5")),
        htmltools::div(class = "mt-2", shiny::actionButton(ns("compare"), "Fit LAD and LSQ", class = "btn-outline-primary w-100", icon = app_icon("columns-gap"))),
        width = 350
      ),
      shiny::uiOutput(ns("status_message")),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        bslib::value_box(title = "Intervals", value = shiny::textOutput(ns("n_intervals"), inline = TRUE), showcase = app_icon("calendar3"), theme = "primary"),
        bslib::value_box(title = "Observed total", value = shiny::textOutput(ns("observed_total"), inline = TRUE), showcase = app_icon("people"), theme = "info"),
        bslib::value_box(title = "Lower MAE", value = shiny::textOutput(ns("winner"), inline = TRUE), showcase = app_icon("trophy"), theme = "success"),
        bslib::value_box(title = "Mechanism", value = shiny::textOutput(ns("mechanism"), inline = TRUE), showcase = app_icon("exclamation-diamond"), theme = "warning")
      ),
      bslib::navset_card_tab(
        full_screen = TRUE,
        bslib::nav_panel("Simulated series", plotly::plotlyOutput(ns("simulation_plot"), height = "480px")),
        bslib::nav_panel("LAD-LSQ fits", plotly::plotlyOutput(ns("comparison_plot"), height = "480px")),
        bslib::nav_panel("Comparison metrics", DT::DTOutput(ns("metrics_table"))),
        bslib::nav_panel("Parameter recovery", DT::DTOutput(ns("recovery_table"))),
        bslib::nav_panel("Data", DT::DTOutput(ns("data_table")))
      ),
      bslib::card(
        bslib::card_header("Export simulation"),
        bslib::card_body(
          htmltools::div(class = "download-row",
            shiny::downloadButton(ns("download_data"), "Simulated data", class = "btn-outline-primary"),
            shiny::downloadButton(ns("download_metrics"), "Comparison metrics", class = "btn-outline-primary")
          )
        )
      )
    )
  )
}

simulation_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    simulation_value <- shiny::reactiveVal(NULL)
    comparison_value <- shiny::reactiveVal(NULL)
    truth_value <- shiny::reactiveVal(NULL)
    design_value <- shiny::reactiveVal(NULL)
    error_value <- shiny::reactiveVal(NULL)

    output$parameter_ui <- shiny::renderUI(simulation_parameter_ui(input$driver, ns))

    shiny::observeEvent(input$simulate, {
      error_value(NULL)
      simulation_value(NULL)
      comparison_value(NULL)
      truth_value(NULL)
      design_value(NULL)
      tryCatch({
        parameters <- simulation_parameters(input, input$driver)
        times <- seq_len(as.integer(input$intervals)) * as.numeric(input$interval_length)
        settings <- ladseir::ladseir_settings(population = input$population, starting_values = input$starts, maximum_iterations = input$iterations)
        simulation <- ladseir::simulate_ladseir(
          parameters = parameters,
          interval_end_times = times,
          driver = input$driver,
          size = input$size,
          seed = as.integer(input$seed),
          settings = settings
        )
        simulation <- apply_reporting_anomaly(simulation, input$anomaly, seed = as.integer(input$seed) + 1L)
        truth_value(parameters)
        design_value(list(
          driver = input$driver, population = input$population, anomaly = input$anomaly,
          seed = as.integer(input$seed), interval_length = input$interval_length
        ))
        simulation_value(simulation)
      }, error = function(e) error_value(friendly_error(e)))
    })

    shiny::observeEvent(input$compare, {
      error_value(NULL)
      if (is.null(simulation_value())) {
        error_value("Simulate a dataset before fitting LAD and LSQ.")
        return()
      }
      tryCatch({
        simulation <- simulation_value()
        design <- design_value()
        settings <- make_settings(design$population, input$starts, input$iterations)
        comparison <- shiny::withProgress(message = "Fitting LAD and LSQ to simulated data", value = 0.15, {
          result <- ladseir::compare_lad_lsq(
            observed = simulation$observed_anomalous,
            interval_end_times = simulation$interval_end_time,
            driver = design$driver,
            settings = settings,
            seed = design$seed
          )
          shiny::incProgress(0.75, detail = "Preparing parameter recovery")
          result
        })
        comparison_value(comparison)
      }, error = function(e) error_value(friendly_error(e)))
    })

    output$status_message <- shiny::renderUI({
      if (!is.null(error_value())) return(htmltools::div(class = "alert alert-danger", app_icon("exclamation-triangle"), error_value()))
      if (is.null(simulation_value())) return(empty_state("Simulation laboratory ready", "Configure a synthetic epidemic and generate reporting-interval incidence.", "dice-5"))
      if (is.null(comparison_value())) return(htmltools::div(class = "alert alert-info", app_icon("info-circle"), "Simulation completed. Run the paired LAD-LSQ fit to evaluate calibration sensitivity."))
      htmltools::div(class = "alert alert-success", app_icon("check-circle"), "Simulation and paired calibration completed successfully.")
    })

    output$n_intervals <- shiny::renderText(if (is.null(simulation_value())) "-" else nrow(simulation_value()))
    output$observed_total <- shiny::renderText(if (is.null(simulation_value())) "-" else format_count(sum(simulation_value()$observed_anomalous)))
    output$mechanism <- shiny::renderText({
      design <- design_value()
      if (is.null(design)) "-" else ANOMALY_LABELS[[design$anomaly]]
    })
    output$winner <- shiny::renderText({
      if (is.null(comparison_value())) return("-")
      metrics <- comparison_metrics_frame(comparison_value())
      metrics$loss[which.min(metrics$MAE)]
    })

    output$simulation_plot <- plotly::renderPlotly({
      shiny::req(simulation_value())
      as_interactive_plot(plot_simulation_result(simulation_value()), tooltip = c("x", "y", "colour"))
    })

    output$comparison_plot <- plotly::renderPlotly({
      shiny::req(comparison_value())
      as_interactive_plot(plot_comparison_fit(comparison_value()), tooltip = c("x", "y", "colour"))
    })

    metrics_data <- shiny::reactive({
      shiny::req(comparison_value())
      comparison_metrics_frame(comparison_value())
    })

    output$metrics_table <- DT::renderDT({
      table <- metrics_data()
      numeric <- vapply(table, is.numeric, logical(1)); table[numeric] <- lapply(table[numeric], signif, digits = 7)
      DT::datatable(table, rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
    })

    recovery_data <- shiny::reactive({
      shiny::req(comparison_value(), truth_value())
      truth <- truth_value()
      do.call(rbind, lapply(c("LAD", "LSQ"), function(loss) {
        estimates <- comparison_value()[[loss]]$parameters
        data.frame(
          loss = loss,
          parameter = names(estimates),
          truth = unname(truth[names(estimates)]),
          estimate = unname(estimates),
          error = unname(estimates - truth[names(estimates)]),
          stringsAsFactors = FALSE
        )
      }))
    })

    output$recovery_table <- DT::renderDT({
      table <- recovery_data()
      table[c("truth", "estimate", "error")] <- lapply(table[c("truth", "estimate", "error")], signif, digits = 7)
      DT::datatable(table, rownames = FALSE, options = list(pageLength = 20, dom = "tip"), class = "compact stripe")
    })

    output$data_table <- DT::renderDT({
      shiny::req(simulation_value())
      table <- simulation_value(); table[c("mean_incidence", "observed", "observed_anomalous")] <- lapply(table[c("mean_incidence", "observed", "observed_anomalous")], round, digits = 3)
      DT::datatable(table, rownames = FALSE, options = list(pageLength = 12, dom = "tip", scrollX = TRUE), class = "compact stripe")
    })

    output$download_data <- shiny::downloadHandler(
      filename = function() "ladseir-simulated-data.csv",
      content = function(file) { shiny::req(simulation_value()); utils::write.csv(simulation_value(), file, row.names = FALSE) }
    )
    output$download_metrics <- shiny::downloadHandler(
      filename = function() "ladseir-simulation-comparison.csv",
      content = function(file) { shiny::req(comparison_value()); utils::write.csv(metrics_data(), file, row.names = FALSE) }
    )

    list(simulation = shiny::reactive(simulation_value()), comparison = shiny::reactive(comparison_value()))
  })
}
