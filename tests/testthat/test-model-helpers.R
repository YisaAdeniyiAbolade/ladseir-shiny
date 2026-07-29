testthat::test_that("calibration metrics are calculated correctly", {
  metrics <- calculate_metrics(c(1, 3, 5), c(1, 2, 7))
  testthat::expect_equal(unname(metrics["MAE"]), 1)
  testthat::expect_equal(unname(metrics["MSE"]), 5 / 3)
  testthat::expect_equal(unname(metrics["RMSE"]), sqrt(5 / 3))
})

testthat::test_that("transmission curves are finite for each family", {
  parameters <- list(
    cosine = c(beta0 = 0.36, sigma = 1 / 7, gamma = 1 / 6.5, a = 0.25, omega = 2 * pi / 120, E0 = 40, I0 = 20),
    exponential = c(beta0 = 0.36, sigma = 1 / 7, gamma = 1 / 6.5, a = 0.60, b = -0.015, E0 = 40, I0 = 20),
    logistic_decline = c(beta0 = 0.50, sigma = 1 / 7, gamma = 1 / 6.5, q = 0.25, k = 0.08, tau = 70, E0 = 40, I0 = 20)
  )
  for (driver in names(parameters)) {
    curve <- transmission_curve(parameters[[driver]], driver, maximum_time = 140)
    testthat::expect_equal(nrow(curve), 300)
    testthat::expect_true(all(is.finite(curve$beta)))
    testthat::expect_true(all(curve$beta > 0))
  }
})

testthat::test_that("fixed parameters are assembled predictably", {
  testthat::expect_equal(fixed_parameter_vector(TRUE, 0.1, FALSE, 0.2), c(sigma = 0.1))
  testthat::expect_equal(fixed_parameter_vector(TRUE, 0.1, TRUE, 0.2), c(sigma = 0.1, gamma = 0.2))
})
