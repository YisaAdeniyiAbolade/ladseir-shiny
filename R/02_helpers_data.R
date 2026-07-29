read_uploaded_csv <- function(path) {
  data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(data) || !ncol(data)) stop("The uploaded CSV is empty.", call. = FALSE)
  data
}


parse_date_flexible <- function(x) {
  if (inherits(x, "Date")) return(x)
  values <- trimws(as.character(x))
  result <- suppressWarnings(as.Date(values))
  formats <- c("%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y")
  for (format in formats) {
    missing <- is.na(result) & nzchar(values)
    if (!any(missing)) break
    result[missing] <- suppressWarnings(as.Date(values[missing], format = format))
  }
  result
}

guess_column <- function(names, patterns) {
  lower <- tolower(names)
  matched <- which(vapply(lower, function(x) any(grepl(paste(patterns, collapse = "|"), x)), logical(1)))
  if (length(matched)) names[matched[1]] else names[1]
}

normalize_uploaded_data <- function(data, cases_column, date_column = NULL,
                                    time_column = NULL, interval_length = DEFAULT_INTERVAL_LENGTH) {
  if (!cases_column %in% names(data)) stop("Select a valid case-count column.", call. = FALSE)
  cases <- suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(data[[cases_column]])), fixed = TRUE)))
  if (anyNA(cases) || any(!is.finite(cases)) || any(cases < 0)) {
    stop("The selected case-count column must contain finite, nonnegative numeric values.", call. = FALSE)
  }

  date <- NULL
  if (!is.null(date_column) && nzchar(date_column) && date_column != "__none__") {
    if (!date_column %in% names(data)) stop("Select a valid date column.", call. = FALSE)
    date <- parse_date_flexible(data[[date_column]])
    if (anyNA(date)) stop("The selected date column could not be parsed as calendar dates.", call. = FALSE)
  }

  if (!is.null(time_column) && nzchar(time_column) && time_column != "__none__") {
    if (!time_column %in% names(data)) stop("Select a valid reporting-time column.", call. = FALSE)
    interval_end_time <- suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(data[[time_column]])), fixed = TRUE)))
  } else if (!is.null(date)) {
    differences <- diff(sort(unique(date)))
    inferred <- if (length(differences)) stats::median(as.numeric(differences)) else interval_length
    interval_end_time <- as.numeric(date - min(date)) + inferred
  } else {
    interval_end_time <- seq_along(cases) * as.numeric(interval_length)
  }

  if (anyNA(interval_end_time) || any(!is.finite(interval_end_time)) || any(interval_end_time <= 0)) {
    stop("Reporting times must be finite and positive.", call. = FALSE)
  }

  order_index <- order(interval_end_time)
  cases <- cases[order_index]
  interval_end_time <- interval_end_time[order_index]
  if (!is.null(date)) date <- date[order_index]
  if (anyDuplicated(interval_end_time)) stop("Reporting times must be unique.", call. = FALSE)

  if (is.null(date)) {
    date <- as.Date(NA) + rep(NA_integer_, length(cases))
  }

  data.frame(
    date = date,
    week = seq_along(cases),
    cases = cases,
    interval_end_time = interval_end_time,
    stringsAsFactors = FALSE
  )
}

validate_analysis_data <- function(data, minimum_rows = 8L) {
  required <- c("cases", "interval_end_time")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("The analysis data are missing required columns: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  if (nrow(data) < minimum_rows) stop("At least ", minimum_rows, " reporting intervals are required.", call. = FALSE)
  if (nrow(data) > 500L) stop("The interactive application accepts at most 500 reporting intervals per analysis.", call. = FALSE)
  if (any(!is.finite(data$cases)) || any(data$cases < 0)) stop("Case counts must be finite and nonnegative.", call. = FALSE)
  if (any(!is.finite(data$interval_end_time)) || any(data$interval_end_time <= 0) ||
      is.unsorted(data$interval_end_time, strictly = TRUE)) {
    stop("Reporting times must be finite, positive, and strictly increasing.", call. = FALSE)
  }
  invisible(TRUE)
}

apply_reporting_anomaly <- function(data, mechanism = "none", seed = DEFAULT_SEED) {
  stopifnot(all(c("mean_incidence", "observed") %in% names(data)))
  mechanism <- match.arg(mechanism, names(ANOMALY_LABELS))
  set.seed(as.integer(seed))
  observed <- as.numeric(data$observed)
  n <- length(observed)
  if (mechanism == "none" || n < 6L) {
    data$observed_anomalous <- observed
    return(data)
  }

  if (mechanism == "isolated_spikes") {
    count <- max(1L, floor(0.10 * n))
    indices <- sample(seq_len(n), count)
    observed[indices] <- round(observed[indices] * 1.75 + pmax(5, 0.15 * max(observed)))
  } else if (mechanism == "backlog_release") {
    start <- max(2L, floor(n * 0.45))
    indices <- start:min(n - 1L, start + 2L)
    withheld <- sum(round(observed[indices] * 0.70))
    observed[indices] <- pmax(0, observed[indices] - round(observed[indices] * 0.70))
    observed[max(indices) + 1L] <- observed[max(indices) + 1L] + withheld
  } else if (mechanism == "underreport_release") {
    start <- max(2L, floor(n * 0.35))
    indices <- start:min(n - 1L, start + max(3L, floor(n * 0.18)))
    original <- observed[indices]
    observed[indices] <- round(0.50 * original)
    observed[max(indices) + 1L] <- observed[max(indices) + 1L] + sum(original - observed[indices])
  } else if (mechanism == "serial_ar1") {
    innovations <- stats::rnorm(n)
    error <- numeric(n)
    for (i in seq_len(n)) error[i] <- if (i == 1L) innovations[i] else 0.75 * error[i - 1L] + innovations[i]
    scale <- pmax(1, 0.20 * sqrt(pmax(data$mean_incidence, 1)))
    observed <- round(pmax(0, observed + error * scale))
  }

  data$observed_anomalous <- observed
  data
}

write_data_template <- function(file) {
  template <- data.frame(
    date = as.Date("2026-01-01") + 7 * 0:11,
    cases = c(4, 7, 11, 18, 27, 41, 56, 63, 58, 46, 33, 21),
    interval_end_time = 7 * 1:12
  )
  utils::write.csv(template, file, row.names = FALSE)
}
