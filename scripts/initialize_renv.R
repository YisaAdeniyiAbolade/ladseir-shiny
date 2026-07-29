if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
if (!file.exists("app.R")) stop("Run this script from the ladseir-shiny repository root.")
renv::init(bare = TRUE)
renv::install(c(
  "shiny", "bslib", "bsicons", "ggplot2", "plotly", "DT",
  "jsonlite", "htmltools", "testthat", "remotes", "rsconnect"
))
renv::install("YisaAdeniyiAbolade/lad-seir-calibration")
renv::snapshot(prompt = FALSE)
message("renv.lock created successfully.")
