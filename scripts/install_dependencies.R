options(repos = c(CRAN = "https://cloud.r-project.org"))

cran_packages <- c(
  "shiny", "bslib", "bsicons", "ggplot2", "plotly", "DT",
  "jsonlite", "htmltools", "testthat", "remotes", "rsconnect", "renv"
)

missing <- cran_packages[!vapply(cran_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing)) {
  message("Installing CRAN dependencies: ", paste(missing, collapse = ", "))
  install.packages(missing, dependencies = c("Depends", "Imports", "LinkingTo"))
} else {
  message("All CRAN dependencies are already installed.")
}

install_ladseir <- !requireNamespace("ladseir", quietly = TRUE)
if (!install_ladseir) {
  description <- utils::packageDescription("ladseir")
  github_source <- identical(description$RemoteType, "github") &&
    identical(description$RemoteUsername, "YisaAdeniyiAbolade") &&
    identical(description$RemoteRepo, "lad-seir-calibration")
  install_ladseir <- utils::packageVersion("ladseir") < "0.1.0" || !github_source
}
if (install_ladseir) {
  message("Installing ladseir from its public GitHub repository...")
  remotes::install_github(
    "YisaAdeniyiAbolade/lad-seir-calibration",
    upgrade = "never",
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
} else {
  message("ladseir ", as.character(utils::packageVersion("ladseir")), " is installed.")
}

message("Application dependencies are ready.")
