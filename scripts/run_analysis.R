args <- commandArgs(trailingOnly=TRUE)
run <- function() {
  if(length(args)!=8L || any(!args[seq(1,8,2)] %in% c("--counts","--samples","--config","--out")))
    stop("Usage: Rscript scripts/run_analysis.R --counts FILE --samples FILE --config YAML --out DIR",call.=FALSE)
  opt <- setNames(as.list(args[seq(2,8,2)]),sub("^--","",args[seq(1,8,2)]))
  if(length(unique(names(opt)))!=4) stop("Each CLI option must occur exactly once.",call.=FALSE)
  dir.create(opt$out,recursive=TRUE,showWarnings=FALSE)
  source("R/000_release.R",encoding="UTF-8"); source("R/core.R",encoding="UTF-8"); source("R/export.R",encoding="UTF-8")
  config <- yaml::read_yaml(opt$config)
  if(!is.null(config$seed)) set.seed(config$seed)
  started <- Sys.time()
  inputs <- read_inputs(opt$counts,opt$samples,config)
  object <- rs_stamp_result(fit_response(inputs$counts,inputs$samples,inputs$config))
  object$provenance <- list(command=c("Rscript","scripts/run_analysis.R",args),
    started_utc=format(started,tz="UTC",usetz=TRUE),
    elapsed_seconds=as.numeric(difftime(Sys.time(),started,units="secs")),
    inputs=lapply(c(opt$counts,opt$samples,opt$config),function(p)
      list(path=normalizePath(p,winslash="/"),sha256=digest::digest(file=p,algo="sha256"))),
    seed=config$seed,exit_code=0L,session=capture.output(sessionInfo()))
  export_response(object,opt$out)
  cat(sprintf("SUCCESS: %d result rows written to %s\n",nrow(object$results),normalizePath(opt$out)))
}
tryCatch(run(),error=function(e) {
  msg <- paste0("Analysis failed: ",conditionMessage(e))
  cat(msg,"\n",file=stderr())
  at <- match("--out",args)
  if(!is.na(at) && at<length(args)) {
    dir.create(args[at+1],recursive=TRUE,showWarnings=FALSE)
    jsonlite::write_json(list(status="failed",exit_code=1L,reason=conditionMessage(e),command=args),
                         file.path(args[at+1],"failure.json"),auto_unbox=TRUE,pretty=TRUE)
  }
  quit(status=1L)
})
