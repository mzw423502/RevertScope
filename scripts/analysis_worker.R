args<-commandArgs(trailingOnly=TRUE);root<-args[1];project<-args[2];id<-args[3]
setwd(root);source('R/core.R',encoding='UTF-8');source('R/projects.R',encoding='UTF-8')
d<-file.path(project,'analyses',id);started<-Sys.time()
check_cancel<-function(){if(file.exists(file.path(d,'cancel_requested.json')))stop(structure(list(message='分析已取消',call=NULL),class=c('revertscope_cancelled','error','condition')))}
.rs_atomic(list(analysis_id=id,worker_pid=Sys.getpid(),temp_directory=tempdir(),systemroot_present=nzchar(Sys.getenv('SYSTEMROOT')),temp_present=nzchar(Sys.getenv('TEMP')),library_path=Sys.getenv('R_LIBS_USER'),locale=Sys.getenv('LC_ALL'),threads=Sys.getenv('OMP_NUM_THREADS')),file.path(d,'worker_pid.json'))
status<-function(stage,state='running',message=NULL) {check_cancel();.rs_atomic(list(analysis_id=id,worker_pid=Sys.getpid(),status=state,stage=stage,message=message,started_utc=format(started,tz='UTC',usetz=TRUE),elapsed_seconds=as.numeric(difftime(Sys.time(),started,units='secs'))),file.path(d,'status.json'))}
tryCatch({
 snap<-.rs_snapshot_read(d);for(f in names(snap$sha256))if(!identical(snap$sha256[[f]],digest::digest(file=file.path(d,f),algo='sha256')))stop('任务输入快照校验失败。')
 config<-yaml::read_yaml(file.path(d,'config.yml'));if(!is.null(config$seed))set.seed(config$seed)
 status('验证');files<-unlist(snap$files);inputs<-read_inputs(file.path(d,files[1]),file.path(d,files[2]),config)
 status('拟合与分类');object<-fit_response(inputs$counts,inputs$samples,inputs$config)
 status('生成成果');object$analysis_id<-id;object<-rs_stamp_result(object);object$provenance<-list(analysis_id=id,seed=config$seed,input_sha256=snap$sha256,command=c('Rscript','scripts/analysis_worker.R','<installation-root>','<project-directory>',id),elapsed_seconds=as.numeric(difftime(Sys.time(),started,units='secs')),session=capture.output(sessionInfo()),exit_code=0L)
 .rs_atomic(object,file.path(d,'result.rds'),TRUE)
 check_cancel();.rs_atomic(list(analysis_id=id,sha256=digest::digest(file=file.path(d,'result.rds'),algo='sha256'),exit_code=0L),file.path(d,'result_ready.json'))
 status('成果已写入，等待退出核验','result_ready')
},error=function(e){if(inherits(e,'revertscope_cancelled')||file.exists(file.path(d,'cancel_requested.json'))){if(file.exists(file.path(d,'complete.json')))unlink(file.path(d,'complete.json'));.rs_atomic(list(analysis_id=id,worker_pid=Sys.getpid(),status='cancelled',stage='已取消'),file.path(d,'status.json'));quit(status=2L)};status('失败','failed',conditionMessage(e));cat(conditionMessage(e),'\n',file=stderr());quit(status=1L)})
