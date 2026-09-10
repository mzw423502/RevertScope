param([Parameter(ValueFromRemainingArguments=$true)][string[]]$AppArgs)
$ErrorActionPreference = 'Stop'
$launchRoot = Split-Path $PSScriptRoot -Parent
Set-Location -LiteralPath $launchRoot
$env:LC_ALL = 'English_United States.utf8'
$env:LC_CTYPE = 'English_United States.utf8'
$env:LANG = 'en_US.UTF-8'
$env:OMP_NUM_THREADS = '1'
$env:OPENBLAS_NUM_THREADS = '1'
$env:MKL_NUM_THREADS = '1'
$env:R_LIBS_USER = Join-Path $launchRoot '.library'
$env:R_LIBS_SITE = Join-Path $launchRoot '.no_site_library'
$env:R_LIBS = $env:R_LIBS_USER
$env:R_PROFILE_USER = 'NUL'
$env:R_ENVIRON_USER = 'NUL'
$rscriptPath = $env:REVERTSCOPE_RSCRIPT
if (!$rscriptPath) {
 $runtimeConfig = Join-Path $launchRoot 'config/runtime_path.txt'
 if (Test-Path -LiteralPath $runtimeConfig) { $rscriptPath = [IO.File]::ReadAllText($runtimeConfig).Trim() }
 elseif (Test-Path -LiteralPath (Join-Path $launchRoot 'runtime/R-4.6.1/bin/Rscript.exe')) { $rscriptPath = Join-Path $launchRoot 'runtime/R-4.6.1/bin/Rscript.exe' }
 else { $rscriptPath = (Get-Command Rscript.exe -ErrorAction SilentlyContinue).Source }
}
if (!$rscriptPath -or !(Test-Path -LiteralPath $rscriptPath)) { throw 'Rscript not found. Install R 4.6.1 and run setup first, or set REVERTSCOPE_RSCRIPT.' }
if (!(Test-Path -LiteralPath (Join-Path $launchRoot '.library/shiny/DESCRIPTION'))) { throw 'Dependencies are not installed. Run the documented setup/restore entry first.' }
$release = Get-Content -LiteralPath (Join-Path $launchRoot 'release_metadata.json') -Raw -Encoding UTF8 | ConvertFrom-Json
Write-Output "RevertScope $($release.software_version) $($release.build_id) - internal candidate"
& $rscriptPath --vanilla scripts/run_app.R --browse @AppArgs
exit $LASTEXITCODE
