args<-commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L,getRversion()==numeric_version('4.6.1'))
.Library.site<-character();.libPaths(c(normalizePath('.library'),.Library),include.site=FALSE)
p<-read.delim('config/dependency_sources.tsv',stringsAsFactors=FALSE)
# packageVersion normalizes hyphens (1.1-3 -> 1.1.3); lock comparison uses raw DESCRIPTION.
lib<-.libPaths()[1]
raw_version<-function(name) {
 d<-file.path(lib,name,'DESCRIPTION')
 if(!file.exists(d))return(NA_character_)
 as.character(read.dcf(d,fields='Version')[1,1])
}
md5_ok<-function(name) {
 # R4.6.1 exposes checkMD5sums(package, dir), not lib.loc.
 tryCatch(isTRUE(tools::checkMD5sums(package=name,dir=file.path(lib,name))),error=function(e)FALSE)
}
archives<-file.path(args[1],paste0(p$package,'_',p$version,'.zip'))
stopifnot(all(file.exists(archives)))
valid_existing<-vapply(seq_len(nrow(p)),function(i)identical(raw_version(p$package[i]),p$version[i])&&md5_ok(p$package[i]),logical(1))
if(any(!valid_existing))install.packages(archives[!valid_existing],repos=NULL,type='win.binary',lib=lib)
for(i in seq_len(nrow(p)))stopifnot(identical(raw_version(p$package[i]),p$version[i]),md5_ok(p$package[i]))
cat('Locked dependencies:',sum(valid_existing),'verified materialized packages reused;',sum(!valid_existing),'installed from verified archives.\n')
release<-jsonlite::read_json('release_metadata.json',simplifyVector=TRUE)
pkg<-file.path('source',paste0('revertscope_',release$software_version,'.tar.gz'))
status<-system2(file.path(R.home('bin'),'R.exe'),c('CMD','INSTALL','--no-multiarch','-l',shQuote(.libPaths()[1]),shQuote(pkg)))
if(status)quit(status=status)
stopifnot(as.character(packageVersion('revertscope',lib.loc=.libPaths()[1]))==release$software_version)
library(revertscope)
for(n in c('shiny','DT','plotly','ragg','svglite','processx','readxl'))stopifnot(requireNamespace(n,quietly=TRUE))
dir.create('logs',showWarnings=FALSE)
jsonlite::write_json(list(status='passed',Rscript=file.path(R.home('bin'),'Rscript.exe'),R_HOME=R.home(),libPaths=.libPaths(),release=release,dependencies_reused=sum(valid_existing),dependencies_installed=sum(!valid_existing),version_comparison='Raw DESCRIPTION Version equals lock exactly; every installed package MD5 checked',packages=as.data.frame(installed.packages(lib.loc=.libPaths()[1],noCache=TRUE))[,c('Package','Version','LibPath')],session=capture.output(sessionInfo()),exit_code=0),'logs/installation.json',pretty=TRUE,auto_unbox=TRUE)
