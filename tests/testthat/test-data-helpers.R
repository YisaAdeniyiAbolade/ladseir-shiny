testthat::test_that("uploaded data are normalized and ordered", {
  input <- data.frame(
    when = c("2026-01-15", "2026-01-01", "2026-01-08"),
    incidence = c(12, 3, 7),
    stringsAsFactors = FALSE
  )
  result <- normalize_uploaded_data(input, "incidence", "when", "__none__", interval_length = 7)
  testthat::expect_equal(result$cases, c(3, 7, 12))
  testthat::expect_equal(result$interval_end_time, c(7, 14, 21))
  extension <- transform(result[rep(3, 5), ], interval_end_time = 28:32)
  testthat::expect_silent(validate_analysis_data(rbind(result, extension)))
})

testthat::test_that("invalid case counts are rejected", {
  input <- data.frame(cases = c(1, -1, 3))
  testthat::expect_error(normalize_uploaded_data(input, "cases"), "nonnegative")
})

testthat::test_that("reporting anomalies preserve series length and nonnegativity", {
  data <- data.frame(interval_end_time = 7 * 1:20, mean_incidence = 10 + 1:20, observed = 10 + 1:20)
  for (mechanism in names(ANOMALY_LABELS)) {
    result <- apply_reporting_anomaly(data, mechanism, seed = 101)
    testthat::expect_equal(nrow(result), nrow(data))
    testthat::expect_true(all(result$observed_anomalous >= 0))
  }
})
