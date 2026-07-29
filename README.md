# LAD-SEIR Interactive Calibration and Forecasting Laboratory

<p align="center"><img src="www/logo.svg" alt="LAD-SEIR logo" width="170"></p>

[![Shiny application validation](https://github.com/YisaAdeniyiAbolade/ladseir-shiny/actions/workflows/app-validation.yaml/badge.svg)](https://github.com/YisaAdeniyiAbolade/ladseir-shiny/actions/workflows/app-validation.yaml)

[![Launch live app](https://img.shields.io/badge/Launch-Live%20App-2ea44f?logo=posit&logoColor=white)](https://019fabd0-6dc7-b38d-7605-290af14e8ceb.share.connect.posit.cloud/)

## Live application

**[Launch the LAD-SEIR Interactive Calibration and Forecasting Laboratory](https://019fabd0-6dc7-b38d-7605-290af14e8ceb.share.connect.posit.cloud/)**

The hosted application runs on Posit Connect Cloud and uses the `ladseir` R package as its computational engine.

The LAD-SEIR Interactive Calibration and Forecasting Laboratory is a professional Shiny application for robust calibration, paired sensitivity analysis, simulation, and forecasting with time-varying susceptible-exposed-infectious-removed models.

The application uses the [`ladseir`](https://github.com/YisaAdeniyiAbolade/lad-seir-calibration) R package as its statistical engine. The package repository remains the authoritative source for the methodology, tested computational functions, manuscript reproducibility workflow, data, and numerical results. This repository provides the interactive application layer.

## Application capabilities

- Load any of the four bundled synthetic RAPIDD Ebola scenarios.
- Upload aggregated reporting-interval incidence from a CSV file.
- Select the calibration window interactively.
- Fit cosine turning-point, exponential, or logistic-decline transmission models.
- Calibrate with least absolute deviations (LAD) or least squares (LSQ).
- Compare LAD and LSQ under identical multistart optimization settings.
- Inspect fitted incidence, residuals, transmission trajectories, parameter estimates, and optimizer runs.
- Generate short-horizon forecasts with negative-binomial or residual-based predictive intervals.
- Simulate epidemic incidence under clean reporting and four reporting-anomaly mechanisms.
- Download analysis-ready data, fitted series, parameter estimates, forecasts, paired metrics, HTML reports, and machine-readable run records.

## Repository relationship

| Repository | Purpose |
|---|---|
| [`lad-seir-calibration`](https://github.com/YisaAdeniyiAbolade/lad-seir-calibration) | Installable `ladseir` R package and complete research reproducibility repository |
| `ladseir-shiny` | Interactive Shiny application, interface tests, deployment scripts, and application documentation |

The Shiny application calls the exported package functions directly:

```r
ladseir_settings()
fit_lad_seir()
compare_lad_lsq()
forecast_lad_seir()
simulate_ladseir()
ladseir_example_data()
```

## Local installation

The application requires R 4.1.0 or later.

From the repository root, install the required packages and the GitHub version of `ladseir`:

```r
source("scripts/install_dependencies.R")
```

On Windows PowerShell, the complete validation workflow is:

```powershell
powershell -ExecutionPolicy Bypass -File ".\scripts\validate_app_windows.ps1"
```

The validator installs missing dependencies, runs the automated tests, constructs the complete Shiny application, and executes calibration and forecasting smoke tests.

## Run the application

From R or RStudio:

```r
shiny::runApp()
```

From PowerShell:

```powershell
powershell -ExecutionPolicy Bypass -File ".\scripts\run_app_windows.ps1"
```

## Input data

Uploaded CSV files require a nonnegative case-count column. The application accepts either:

1. a strictly increasing numeric reporting-time column;
2. a calendar-date column from which reporting times can be inferred; or
3. row order combined with a user-specified reporting-interval length.

A valid template is available at [`sample-data/custom-epidemic-template.csv`](sample-data/custom-epidemic-template.csv) and from the Data page inside the application.

The application is designed for aggregated incidence. Public deployments should not be used for confidential, personally identifiable, or regulated health information.

## Application architecture

```text
User data or bundled RAPIDD scenarios
                │
                ▼
        Shiny modules and validation
                │
                ▼
      ladseir statistical package
                │
                ▼
Calibration · comparison · forecasting · simulation
                │
                ▼
Interactive diagnostics and reproducible downloads
```

The application is organized into independent modules:

```text
app.R
R/
├── 00_config.R
├── 01_helpers_general.R
├── 02_helpers_data.R
├── 03_helpers_plot.R
├── 10_module_home.R
├── 11_module_data.R
├── 12_module_calibration.R
├── 13_module_comparison.R
├── 14_module_forecast.R
├── 15_module_simulation.R
└── 16_module_reproducibility.R
```

## Reproducible environment

After dependencies are installed and the application passes validation, create the project lockfile with:

```r
source("scripts/initialize_renv.R")
```

Commit the resulting `renv.lock` and `renv/activate.R` files to preserve the exact deployment environment.

## Deployment

Configure a Posit hosting account through `rsconnect`, then deploy from the repository root:

```r
source("scripts/deploy_shinyapps.R")
```

The `.rscignore` file restricts the deployment bundle to application runtime files. Authentication tokens and account credentials are never stored in this repository.

## Validation

The GitHub Actions workflow performs the following checks on every push and pull request:

- installs CRAN dependencies;
- installs `ladseir` from its public GitHub repository;
- runs helper and UI unit tests;
- renders the full application object;
- fits a LAD logistic-decline model to a bundled RAPIDD subset; and
- generates a predictive forecast.

## License

This project is released under the GNU General Public License version 3 or later.
