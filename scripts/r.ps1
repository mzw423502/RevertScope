# Project-scoped Windows launcher; fixes inherited Unix locales before R starts.
$env:LC_ALL = 'English_United States.utf8'
$env:LC_CTYPE = 'English_United States.utf8'
$env:LANG = 'en_US.UTF-8'
$env:OMP_NUM_THREADS = '1'
$env:OPENBLAS_NUM_THREADS = '1'
$env:MKL_NUM_THREADS = '1'
$root = Split-Path $PSScriptRoot -Parent
$env:R_LIBS_USER = Join-Path $root '.library'
$env:R_LIBS_SITE = Join-Path $root '.no_site_library'
$env:R_LIBS = $env:R_LIBS_USER
$rExecutable = $env:REVERTSCOPE_RSCRIPT
if (!$rExecutable) { $rExecutable = Join-Path $root 'runtime\R-4.6.1\bin\Rscript.exe' }
if (!(Test-Path -LiteralPath $rExecutable)) {
 $runtimeFile = Join-Path $root 'config/runtime_path.txt'
 if (Test-Path -LiteralPath $runtimeFile) { $rExecutable = [IO.File]::ReadAllText($runtimeFile).Trim() }
 else { $rExecutable = (Get-Command Rscript.exe -ErrorAction Stop).Source }
}
& $rExecutable --vanilla @args
exit $LASTEXITCODE
