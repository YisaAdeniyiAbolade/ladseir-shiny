testthat::test_that("all application pages construct successfully", {
  testthat::expect_s3_class(home_ui("home-test"), "shiny.tag")
  testthat::expect_s3_class(data_ui("data-test"), "shiny.tag")
  testthat::expect_s3_class(calibration_ui("calibration-test"), "shiny.tag")
  testthat::expect_s3_class(comparison_ui("comparison-test"), "shiny.tag")
  testthat::expect_s3_class(forecast_ui("forecast-test"), "shiny.tag")
  testthat::expect_s3_class(simulation_ui("simulation-test"), "shiny.tag")
  testthat::expect_s3_class(reproducibility_ui("reproducibility-test"), "shiny.tag")
})
