APP_VERSION <- "0.1.0"
CORE_REPOSITORY <- "https://github.com/YisaAdeniyiAbolade/lad-seir-calibration"
APP_REPOSITORY <- "https://github.com/YisaAdeniyiAbolade/ladseir-shiny"
DEFAULT_POPULATION <- 4499621
DEFAULT_SEED <- 20260627L
DEFAULT_INTERVAL_LENGTH <- 7

APP_THEME <- bslib::bs_theme(
  version = 5,
  preset = "flatly",
  primary = "#0B4F6C",
  secondary = "#5B6770",
  success = "#2B8A3E",
  info = "#2A7F9E",
  warning = "#B7791F",
  danger = "#B42318",
  bg = "#F6F8FA",
  fg = "#17202A",
  base_font = bslib::font_collection(
    "Inter", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"
  ),
  code_font = bslib::font_collection(
    "SFMono-Regular", "Consolas", "Liberation Mono", "monospace"
  )
)

DRIVER_LABELS <- c(
  cosine = "Cosine turning-point",
  exponential = "Exponential",
  logistic_decline = "Logistic decline"
)

LOSS_LABELS <- c(LAD = "Least absolute deviations (LAD)", LSQ = "Least squares (LSQ)")

PREDICTIVE_METHOD_LABELS <- c(
  negative_binomial = "Negative binomial",
  iid_residual = "IID residual",
  wild = "Wild residual",
  moving_block = "Moving-block residual"
)

ANOMALY_LABELS <- c(
  none = "None",
  isolated_spikes = "Isolated spikes",
  backlog_release = "Backlog release",
  underreport_release = "Temporary underreporting with release",
  serial_ar1 = "Serially correlated reporting error"
)
