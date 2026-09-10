param([Parameter(Mandatory=$true)][string]$Rscript, [string]$Cache='')
$ErrorActionPreference='Stop'
$restoreRoot=Split-Path $PSScriptRoot -Parent
Set-Location -LiteralPath $restoreRoot
$Rscript=(Resolve-Path -LiteralPath $Rscript).Path
if (!$Cache) { $Cache=Join-Path $restoreRoot '.download-cache' }
New-Item -ItemType Directory -Force -Path $Cache | Out-Null
$packages=Import-Csv -LiteralPath (Join-Path $restoreRoot 'config/dependency_sources.tsv') -Delimiter "`t"
$records=@()
foreach($p in $packages) {
 $dest=Join-Path $Cache ($p.package+'_'+$p.version+'.zip')
 if (!(Test-Path -LiteralPath $dest) -or (Get-FileHash -LiteralPath $dest -Algorithm SHA256).Hash.ToLower() -ne $p.sha256) {
  Invoke-WebRequest -Uri $p.source_url -OutFile ($dest+'.partial') -UseBasicParsing
  if ((Get-FileHash -LiteralPath ($dest+'.partial') -Algorithm SHA256).Hash.ToLower() -ne $p.sha256) { throw "Dependency SHA mismatch: $($p.package)" }
  Move-Item -LiteralPath ($dest+'.partial') -Destination $dest -Force
 }
 $records+=@{package=$p.package;version=$p.version;sha256=$p.sha256;source=$p.source_url}
}
New-Item -ItemType Directory -Force -Path (Join-Path $restoreRoot '.library') | Out-Null
$env:R_LIBS_USER=Join-Path $restoreRoot '.library'
$env:R_LIBS_SITE=Join-Path $restoreRoot '.no_site_library'
$env:R_LIBS=$env:R_LIBS_USER
$env:LC_ALL='English_United States.utf8'
$env:LC_CTYPE=$env:LC_ALL
$env:LANG='en_US.UTF-8'
$env:OMP_NUM_THREADS='1'
$env:OPENBLAS_NUM_THREADS='1'
$env:MKL_NUM_THREADS='1'
& $Rscript --vanilla scripts/install_release_dependencies.R $Cache
if ($LASTEXITCODE -ne 0) { throw "Installation failed: $LASTEXITCODE" }
[IO.File]::WriteAllText((Join-Path $restoreRoot 'config/runtime_path.txt'),$Rscript,[Text.UTF8Encoding]::new($false))
New-Item -ItemType Directory -Force -Path (Join-Path $restoreRoot 'logs') | Out-Null
$records | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $restoreRoot 'logs/dependency_sources.json') -Encoding UTF8
Write-Output 'Installation verified. Launch using the root CMD file. First preparation may use the network; installed application runs locally.'
