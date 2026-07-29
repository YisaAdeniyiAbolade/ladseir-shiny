required <- c(
  "shiny", "bslib", "bsicons", "ggplot2", "plotly", "DT",
  "jsonlite", "htmltools", "ladseir"
)
missing <- required[
  !vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing)) {
  stop("Missing test dependencies: ", paste(missing, collapse = ", "), call. = FALSE)
}

# testthat evaluates helper files from tests/testthat. Locate the application
# root explicitly so the test suite behaves consistently in local, CI, and
# deployment environments.
candidate_roots <- unique(c(
  normalizePath(getwd(), winslash = "/", mustWork = FALSE),
  normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
))
valid_root <- vapply(
  candidate_roots,
  function(path) file.exists(file.path(path, "app.R")) && dir.exists(file.path(path, "R")),
  logical(1)
)
if (!any(valid_root)) {
  stop("The ladseir-shiny application root could not be located for testing.", call. = FALSE)
}
project_root <- candidate_roots[which(valid_root)[1]]
r_files <- sort(list.files(
  file.path(project_root, "R"),
  pattern = "\\.R$",
  full.names = TRUE
))
if (!length(r_files)) {
  stop("No application R source files were found for testing.", call. = FALSE)
}

invisible(lapply(r_files, sys.source, envir = environment()))
