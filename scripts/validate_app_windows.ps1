$ErrorActionPreference = "Stop"

if (-not (Test-Path "app.R")) {
    throw "Run this script from the ladseir-shiny repository root."
}

$rscriptCommand = Get-Command Rscript.exe -ErrorAction SilentlyContinue
if ($null -eq $rscriptCommand) {
    $rscriptPath = Get-ChildItem "C:\Program Files\R\R-*\bin\Rscript.exe", "C:\Program Files\R\R-*\bin\x64\Rscript.exe" -ErrorAction SilentlyContinue |
        Sort-Object FullName -Descending |
        Select-Object -First 1 -ExpandProperty FullName
    if (-not $rscriptPath) {
        throw "Rscript.exe was not found. Install R before running application validation."
    }
} else {
    $rscriptPath = $rscriptCommand.Source
}

$rBin = Split-Path $rscriptPath -Parent
$env:Path = "$rBin;$env:Path"

Write-Host "1. Installing application dependencies..." -ForegroundColor Cyan
& $rscriptPath scripts/install_dependencies.R
if ($LASTEXITCODE -ne 0) { throw "Dependency installation failed." }

Write-Host "2. Running application validation..." -ForegroundColor Cyan
& $rscriptPath scripts/validate_app.R
if ($LASTEXITCODE -ne 0) { throw "Application validation failed." }

Write-Host "ladseir Shiny application validation completed successfully." -ForegroundColor Green
