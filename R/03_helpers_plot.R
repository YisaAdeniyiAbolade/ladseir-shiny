app_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#0B4F6C"),
      plot.subtitle = ggplot2::element_text(color = "#5B6770"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

as_interactive_plot <- function(plot, tooltip = c("x", "y")) {
  plotly::ggplotly(plot, tooltip = tooltip) |>
    plotly::layout(
      hoverlabel = list(bgcolor = "white", font = list(color = "#17202A")),
      margin = list(l = 60, r = 25, b = 55, t = 65)
    ) |>
    plotly::config(displaylogo = FALSE, responsive = TRUE)
}

plot_incidence_data <- function(data, calibration_end = NULL) {
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = interval_end_time, y = cases)) +
    ggplot2::geom_line(linewidth = 0.8, color = "#2A7F9E") +
    ggplot2::geom_point(size = 2.4, color = "#0B4F6C") +
    ggplot2::labs(
      title = "Reporting-interval incidence",
      x = "Interval end time (days)",
      y = "Reported cases"
    ) +
    app_plot_theme()
  if (!is.null(calibration_end) && calibration_end < nrow(data)) {
    boundary <- data$interval_end_time[calibration_end]
    plot <- plot +
      ggplot2::geom_vline(xintercept = boundary, linetype = "dashed", color = "#B7791F") +
      ggplot2::annotate("text", x = boundary, y = Inf, label = " Calibration endpoint", hjust = 0, vjust = 1.5, color = "#8A5A12")
  }
  plot
}

plot_fit_result <- function(fit) {
  data <- fit_frame(fit)
  ggplot2::ggplot(data, ggplot2::aes(x = interval_end_time)) +
    ggplot2::geom_line(ggplot2::aes(y = observed, color = "Observed"), linewidth = 0.75) +
    ggplot2::geom_point(ggplot2::aes(y = observed, color = "Observed"), size = 2.2) +
    ggplot2::geom_line(ggplot2::aes(y = fitted, color = "Fitted"), linewidth = 1.1) +
    ggplot2::scale_color_manual(values = c(Observed = "#17202A", Fitted = "#0B7285")) +
    ggplot2::labs(
      title = paste(DRIVER_LABELS[[fit$driver]], fit$loss, "calibration"),
      subtitle = "Observed and fitted reporting-interval incidence",
      x = "Interval end time (days)", y = "Cases", color = NULL
    ) +
    app_plot_theme()
}

plot_residual_result <- function(fit) {
  data <- fit_frame(fit)
  ggplot2::ggplot(data, ggplot2::aes(x = interval_end_time, y = residual)) +
    ggplot2::geom_hline(yintercept = 0, color = "#5B6770", linewidth = 0.6) +
    ggplot2::geom_segment(ggplot2::aes(xend = interval_end_time, yend = 0), color = "#7CA6B8") +
    ggplot2::geom_point(size = 2.3, color = "#0B4F6C") +
    ggplot2::labs(title = "Calibration residuals", x = "Interval end time (days)", y = "Observed - fitted") +
    app_plot_theme()
}

plot_transmission_result <- function(fit) {
  data <- transmission_curve(fit$parameters, fit$driver, max(fit$interval_end_times))
  ggplot2::ggplot(data, ggplot2::aes(x = time, y = beta)) +
    ggplot2::geom_line(linewidth = 1.15, color = "#0B7285") +
    ggplot2::labs(title = "Estimated transmission trajectory", x = "Time (days)", y = "Transmission rate β(t)") +
    app_plot_theme()
}

plot_comparison_fit <- function(comparison) {
  observed <- data.frame(
    interval_end_time = comparison$LAD$interval_end_times,
    value = comparison$LAD$observations,
    series = "Observed"
  )
  fits <- rbind(
    data.frame(interval_end_time = comparison$LAD$interval_end_times, value = comparison$LAD$fitted, series = "LAD"),
    data.frame(interval_end_time = comparison$LSQ$interval_end_times, value = comparison$LSQ$fitted, series = "LSQ")
  )
  ggplot2::ggplot() +
    ggplot2::geom_line(data = observed, ggplot2::aes(interval_end_time, value, color = series), linewidth = 0.75) +
    ggplot2::geom_point(data = observed, ggplot2::aes(interval_end_time, value, color = series), size = 2) +
    ggplot2::geom_line(data = fits, ggplot2::aes(interval_end_time, value, color = series), linewidth = 1.05) +
    ggplot2::scale_color_manual(values = c(Observed = "#17202A", LAD = "#0B7285", LSQ = "#B7791F")) +
    ggplot2::labs(title = "Paired LAD-LSQ calibration", subtitle = "Both criteria use identical multistart values", x = "Interval end time (days)", y = "Cases", color = NULL) +
    app_plot_theme()
}

plot_forecast_result <- function(fit, forecast) {
  history <- fit_frame(fit)
  ggplot2::ggplot() +
    ggplot2::geom_line(data = history, ggplot2::aes(interval_end_time, observed, color = "Observed"), linewidth = 0.7) +
    ggplot2::geom_point(data = history, ggplot2::aes(interval_end_time, observed, color = "Observed"), size = 2) +
    ggplot2::geom_line(data = history, ggplot2::aes(interval_end_time, fitted, color = "Fitted"), linewidth = 1) +
    ggplot2::geom_ribbon(data = forecast, ggplot2::aes(interval_end_time, ymin = lower, ymax = upper), fill = "#2A7F9E", alpha = 0.18) +
    ggplot2::geom_line(data = forecast, ggplot2::aes(interval_end_time, median, color = "Forecast"), linewidth = 1.1) +
    ggplot2::geom_vline(xintercept = max(history$interval_end_time), linetype = "dashed", color = "#5B6770") +
    ggplot2::scale_color_manual(values = c(Observed = "#17202A", Fitted = "#0B7285", Forecast = "#B7791F")) +
    ggplot2::labs(title = "Forecast with predictive interval", x = "Interval end time (days)", y = "Cases", color = NULL) +
    app_plot_theme()
}

plot_simulation_result <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = interval_end_time)) +
    ggplot2::geom_line(ggplot2::aes(y = mean_incidence, color = "Model mean"), linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(y = observed_anomalous, color = "Observed"), size = 2) +
    ggplot2::scale_color_manual(values = c("Model mean" = "#0B7285", Observed = "#17202A")) +
    ggplot2::labs(title = "Simulated reporting-interval incidence", x = "Interval end time (days)", y = "Cases", color = NULL) +
    app_plot_theme()
}
