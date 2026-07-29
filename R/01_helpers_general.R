`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

app_icon <- function(name, size = NULL) {
  bsicons::bs_icon(name, size = size %||% "1em")
}

format_count <- function(x, digits = 0L) {
  if (!length(x) || is.na(x) || !is.finite(x)) return("-")
  format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_metric <- function(x, digits = 3L) {
  if (!length(x) || is.na(x) || !is.finite(x)) return("-")
  formatC(x, format = "fg", digits = digits, flag = "#")
}

calculate_metrics <- function(observed, fitted) {
  observed <- as.numeric(observed)
  fitted <- as.numeric(fitted)
  residuals <- observed - fitted
  c(
    MAE = mean(abs(residuals)),
    RMSE = sqrt(mean(residuals^2)),
    MSE = mean(residuals^2),
    Bias = mean(residuals)
  )
}

safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "-", x)
  gsub("-+", "-", x)
}

friendly_error <- function(error) {
  message <- conditionMessage(error)
  known <- c(
    "All optimization starts failed" = "The optimizer could not identify a valid solution. Increase the number of starts or review the selected data and model settings.",
    "Invalid parameter vector" = "The selected parameter values are outside the feasible model region.",
    "strictly increasing" = "Reporting times must be unique and strictly increasing.",
    "nonnegative" = "Case counts must be nonnegative.",
    "outside its configured bounds" = "A fixed parameter is outside the model's admissible range."
  )
  for (pattern in names(known)) {
    if (grepl(pattern, message, fixed = TRUE)) return(known[[pattern]])
  }
  paste("The requested analysis could not be completed:", message)
}

empty_state <- function(title, message, icon = "info-circle") {
  htmltools::div(
    class = "empty-state",
    app_icon(icon, "2rem"),
    htmltools::h4(title),
    htmltools::p(message)
  )
}

status_badge <- function(convergence) {
  if (is.null(convergence) || !length(convergence)) return(htmltools::span(class = "badge text-bg-secondary", "Not run"))
  if (identical(as.integer(convergence), 0L)) {
    htmltools::span(class = "badge text-bg-success", "Converged")
  } else {
    htmltools::span(class = "badge text-bg-warning", paste("Code", convergence))
  }
}

make_settings <- function(population, starts, iterations) {
  ladseir::ladseir_settings(
    population = as.numeric(population),
    starting_values = as.integer(starts),
    maximum_iterations = as.integer(iterations),
    cores = 1L
  )
}

fixed_parameter_vector <- function(fix_sigma, sigma, fix_gamma, gamma) {
  values <- numeric(0)
  if (isTRUE(fix_sigma)) values <- c(values, sigma = as.numeric(sigma))
  if (isTRUE(fix_gamma)) values <- c(values, gamma = as.numeric(gamma))
  values
}

transmission_curve <- function(parameters, driver, maximum_time, points = 300L) {
  time <- seq(0, maximum_time, length.out = points)
  beta0 <- unname(parameters["beta0"])
  beta <- switch(
    driver,
    cosine = beta0 * (1 + unname(parameters["a"]) * cos(unname(parameters["omega"]) * time)),
    exponential = beta0 * (1 + unname(parameters["a"]) * exp(unname(parameters["b"]) * time)),
    logistic_decline = {
      q <- unname(parameters["q"])
      k <- unname(parameters["k"])
      tau <- unname(parameters["tau"])
      beta0 * (q + (1 - q) / (1 + exp(k * (time - tau))))
    }
  )
  data.frame(time = time, beta = beta)
}

fit_frame <- function(fit) {
  data.frame(
    interval_end_time = fit$interval_end_times,
    observed = fit$observations,
    fitted = fit$fitted,
    residual = fit$residuals,
    stringsAsFactors = FALSE
  )
}

parameter_frame <- function(fit) {
  data.frame(
    parameter = names(fit$parameters),
    estimate = unname(fit$parameters),
    fixed = names(fit$parameters) %in% names(fit$fixed_parameters),
    stringsAsFactors = FALSE
  )
}

comparison_metrics_frame <- function(comparison) {
  do.call(rbind, lapply(c("LAD", "LSQ"), function(loss) {
    fit <- comparison[[loss]]
    metrics <- calculate_metrics(fit$observations, fit$fitted)
    data.frame(
      loss = loss,
      objective = fit$objective,
      convergence = fit$convergence,
      MAE = unname(metrics["MAE"]),
      RMSE = unname(metrics["RMSE"]),
      Bias = unname(metrics["Bias"]),
      stringsAsFactors = FALSE
    )
  }))
}

write_fit_html <- function(fit, file, title = "LAD-SEIR calibration report", metadata = list()) {
  metrics <- calculate_metrics(fit$observations, fit$fitted)
  parameters <- parameter_frame(fit)
  metadata_rows <- if (length(metadata)) {
    htmltools::tags$dl(class = "report-metadata", lapply(names(metadata), function(name) {
      htmltools::tagList(htmltools::tags$dt(name), htmltools::tags$dd(as.character(metadata[[name]])))
    }))
  }
  page <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title(title),
      htmltools::tags$style(htmltools::HTML(
        "body{font-family:Segoe UI,Arial,sans-serif;max-width:1000px;margin:40px auto;color:#17202A;line-height:1.5}h1,h2{color:#0B4F6C}table{border-collapse:collapse;width:100%;margin:16px 0}th,td{padding:8px 10px;border-bottom:1px solid #D9E2E8;text-align:left}.metric{display:inline-block;padding:12px 18px;margin:6px;background:#F0F5F8;border-radius:8px}.report-metadata{display:grid;grid-template-columns:180px 1fr;gap:4px 16px}dt{font-weight:700}"
      ))
    ),
    htmltools::tags$body(
      htmltools::tags$h1(title),
      metadata_rows,
      htmltools::tags$h2("Model specification"),
      htmltools::tags$p(paste("Transmission family:", DRIVER_LABELS[[fit$driver]])),
      htmltools::tags$p(paste("Calibration criterion:", fit$loss)),
      htmltools::tags$p(paste("Convergence code:", fit$convergence)),
      htmltools::tags$h2("Calibration metrics"),
      lapply(names(metrics), function(name) htmltools::tags$div(class = "metric", htmltools::tags$b(name), htmltools::tags$br(), format_metric(metrics[[name]], 6))),
      htmltools::tags$h2("Parameter estimates"),
      htmltools::tags$table(
        htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th("Parameter"), htmltools::tags$th("Estimate"), htmltools::tags$th("Fixed"))),
        htmltools::tags$tbody(lapply(seq_len(nrow(parameters)), function(i) {
          htmltools::tags$tr(
            htmltools::tags$td(parameters$parameter[i]),
            htmltools::tags$td(format_metric(parameters$estimate[i], 8)),
            htmltools::tags$td(if (parameters$fixed[i]) "Yes" else "No")
          )
        }))
      ),
      htmltools::tags$p(class = "footer", paste("Generated with ladseir", as.character(utils::packageVersion("ladseir")), "and LAD-SEIR Interactive Laboratory", APP_VERSION))
    )
  )
  htmltools::save_html(page, file = file)
}
