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
        throw "Rscript.exe was not found. Install R before running the application."
    }
} else {
    $rscriptPath = $rscriptCommand.Source
}

& $rscriptPath -e "shiny::runApp('.', launch.browser = TRUE)"
