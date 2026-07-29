comparison_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "app-page",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Paired comparison",
        calibration_controls(ns, include_loss = FALSE, button_id = "run", button_label = "Compare LAD and LSQ"),
        htmltools::p(class = "help-text mt-3", "Both criteria use the same feasible starting values, population, fixed parameters, and optimizer settings."),
        width = 340
      ),
      shiny::uiOutput(ns("status_message")),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        bslib::value_box(title = "LAD MAE", value = shiny::textOutput(ns("lad_mae"), inline = TRUE), showcase = app_icon("shield-check"), theme = "primary"),
        bslib::value_box(title = "LSQ MAE", value = shiny::textOutput(ns("lsq_mae"), inline = TRUE), showcase = app_icon("activity"), theme = "warning"),
        bslib::value_box(title = "Lower MAE", value = shiny::textOutput(ns("winner"), inline = TRUE), showcase = app_icon("trophy"), theme = "success"),
        bslib::value_box(title = "Shared starts", value = shiny::textOutput(ns("n_starts"), inline = TRUE), showcase = app_icon("shuffle"), theme = "info")
      ),
      bslib::navset_card_tab(
        full_screen = TRUE,
        bslib::nav_panel("Fitted trajectories", plotly::plotlyOutput(ns("fit_plot"), height = "480px")),
        bslib::nav_panel("Metrics", DT::DTOutput(ns("metrics_table"))),
        bslib::nav_panel("Parameters", DT::DTOutput(ns("parameter_table"))),
        bslib::nav_panel("Residual comparison", plotly::plotlyOutput(ns("residual_plot"), height = "480px"))
      ),
      bslib::card(
        bslib::card_header("Export paired results"),
        bslib::card_body(
          htmltools::div(
            class = "download-row",
            shiny::downloadButton(ns("download_metrics"), "Metrics", class = "btn-outline-primary"),
            shiny::downloadButton(ns("download_fits"), "Fitted series", class = "btn-outline-primary"),
            shiny::downloadButton(ns("download_parameters"), "Parameters", class = "btn-outline-primary")
          )
        )
      )
    )
  )
}

comparison_server <- function(id, training_data) {
  shiny::moduleServer(id, function(input, output, session) {
    comparison_value <- shiny::reactiveVal(NULL)
    error_value <- shiny::reactiveVal(NULL)

    shiny::observeEvent(training_data(), {
      comparison_value(NULL)
      error_value(NULL)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      comparison_value(NULL)
      error_value(NULL)
      data <- training_data()
      tryCatch({
        validate_analysis_data(data)
        settings <- make_settings(input$population, input$starts, input$iterations)
        fixed <- fixed_parameter_vector(input$fix_sigma, input$sigma, input$fix_gamma, input$gamma)
        comparison <- shiny::withProgress(message = "Running paired LAD-LSQ calibration", value = 0.10, {
          shiny::incProgress(0.25, detail = "Generating shared starting values")
          result <- ladseir::compare_lad_lsq(
            observed = data$cases,
            interval_end_times = data$interval_end_time,
            driver = input$driver,
            settings = settings,
            fixed_parameters = fixed,
            seed = as.integer(input$seed)
          )
          shiny::incProgress(0.60, detail = "Preparing comparison outputs")
          result
        })
        comparison_value(comparison)
      }, error = function(e) error_value(friendly_error(e)))
    })

    output$status_message <- shiny::renderUI({
      if (!is.null(error_value())) return(htmltools::div(class = "alert alert-danger", app_icon("exclamation-triangle"), error_value()))
      if (is.null(comparison_value())) return(empty_state("Paired analysis ready", "Select a transmission family and run the comparison.", "columns-gap"))
      metrics <- comparison_metrics_frame(comparison_value())
      htmltools::div(class = "alert alert-success", app_icon("check-circle"), paste("Paired calibration completed. Lower MAE:", metrics$loss[which.min(metrics$MAE)], "."))
    })

    metrics <- shiny::reactive({
      shiny::req(comparison_value())
      comparison_metrics_frame(comparison_value())
    })

    output$lad_mae <- shiny::renderText(if (is.null(comparison_value())) "-" else format_metric(metrics()$MAE[metrics()$loss == "LAD"], 5))
    output$lsq_mae <- shiny::renderText(if (is.null(comparison_value())) "-" else format_metric(metrics()$MAE[metrics()$loss == "LSQ"], 5))
    output$winner <- shiny::renderText(if (is.null(comparison_value())) "-" else metrics()$loss[which.min(metrics()$MAE)])
    output$n_starts <- shiny::renderText(if (is.null(comparison_value())) "-" else length(comparison_value()$shared_starting_values))

    output$fit_plot <- plotly::renderPlotly({
      shiny::req(comparison_value())
      as_interactive_plot(plot_comparison_fit(comparison_value()), tooltip = c("x", "y", "colour"))
    })

    output$metrics_table <- DT::renderDT({
      table <- metrics()
      numeric <- vapply(table, is.numeric, logical(1))
      table[numeric] <- lapply(table[numeric], signif, digits = 7)
      DT::datatable(table, rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
    })

    parameter_data <- shiny::reactive({
      comparison <- comparison_value(); shiny::req(comparison)
      lad <- parameter_frame(comparison$LAD); lad$loss <- "LAD"
      lsq <- parameter_frame(comparison$LSQ); lsq$loss <- "LSQ"
      result <- rbind(lad, lsq)
      result[, c("loss", "parameter", "estimate", "fixed")]
    })

    output$parameter_table <- DT::renderDT({
      table <- parameter_data(); table$estimate <- signif(table$estimate, 8)
      DT::datatable(table, rownames = FALSE, options = list(pageLength = 20, dom = "tip"), class = "compact stripe")
    })

    output$residual_plot <- plotly::renderPlotly({
      comparison <- comparison_value(); shiny::req(comparison)
      data <- rbind(
        data.frame(interval_end_time = comparison$LAD$interval_end_times, residual = comparison$LAD$residuals, loss = "LAD"),
        data.frame(interval_end_time = comparison$LSQ$interval_end_times, residual = comparison$LSQ$residuals, loss = "LSQ")
      )
      plot <- ggplot2::ggplot(data, ggplot2::aes(interval_end_time, residual, color = loss)) +
        ggplot2::geom_hline(yintercept = 0, color = "#5B6770") +
        ggplot2::geom_line(linewidth = 0.7) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_color_manual(values = c(LAD = "#0B7285", LSQ = "#B7791F")) +
        ggplot2::labs(title = "Residual comparison", x = "Interval end time (days)", y = "Observed - fitted", color = NULL) +
        app_plot_theme()
      as_interactive_plot(plot, tooltip = c("x", "y", "colour"))
    })

    output$download_metrics <- shiny::downloadHandler(
      filename = function() "ladseir-lad-lsq-metrics.csv",
      content = function(file) utils::write.csv(metrics(), file, row.names = FALSE)
    )
    output$download_parameters <- shiny::downloadHandler(
      filename = function() "ladseir-lad-lsq-parameters.csv",
      content = function(file) utils::write.csv(parameter_data(), file, row.names = FALSE)
    )
    output$download_fits <- shiny::downloadHandler(
      filename = function() "ladseir-lad-lsq-fitted-series.csv",
      content = function(file) {
        comparison <- comparison_value(); shiny::req(comparison)
        data <- data.frame(
          interval_end_time = comparison$LAD$interval_end_times,
          observed = comparison$LAD$observations,
          fitted_LAD = comparison$LAD$fitted,
          fitted_LSQ = comparison$LSQ$fitted,
          residual_LAD = comparison$LAD$residuals,
          residual_LSQ = comparison$LSQ$residuals
        )
        utils::write.csv(data, file, row.names = FALSE)
      }
    )

    list(comparison = shiny::reactive(comparison_value()))
  })
}
