if(!exists('rs_release',mode='function'))source('R/000_release.R',encoding='UTF-8')
# Local project persistence and controlled workers; statistical core is unchanged.
.rs_project_path <- function(project) if(is.list(project)) project$path else project
.rs_workspace <- function(root) normalizePath(file.path(root,".."),winslash="/",mustWork=TRUE)
.rs_inside <- function(path,base,mustWork=TRUE) {
 p <- normalizePath(path,winslash="/",mustWork=mustWork); b <- normalizePath(base,winslash="/",mustWork=TRUE)
 if(!startsWith(tolower(paste0(p,"/")),tolower(paste0(b,"/")))) stop("[PROJECT_PATH] 项目路径必须位于当前授权工作区。",call.=FALSE)
 p
}
.rs_atomic <- function(value,path,rds=FALSE) {
 dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
 # Same-directory temporary files ensure the rename stays on one filesystem.
 # Never unlink the destination: readers must see the complete old or new file.
 tmp<-tempfile(pattern=paste0(basename(path),".tmp-",Sys.getpid(),"-"),tmpdir=dirname(path))
 on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
 if(rds)saveRDS(value,tmp,version=3)else jsonlite::write_json(value,tmp,auto_unbox=TRUE,pretty=TRUE,null="null",na="null")
 problem<-NULL
 for(attempt in seq_len(200L)) {
  problem<-tryCatch({fs::file_move(tmp,path);NULL},error=function(e)e)
  if(is.null(problem))return(invisible(path))
  # Windows readers or virus scanners can briefly prevent replacement. Keep the
  # old destination intact while waiting, rather than creating a missing window.
  if(!file.exists(tmp))stop("项目原子替换状态异常；原有结果未主动删除。",call.=FALSE)
  Sys.sleep(.005)
 }
 stop(paste("项目文件暂时被占用，保存未完成；旧版本保持完整：",conditionMessage(problem)),call.=FALSE)
}
.rs_read_retry <- function(reader) {
 # Windows can transiently deny a new open during replacement; readers retry
 # without modifying either the destination or a recovery copy.
 problem<-NULL
 for(attempt in seq_len(200L)) {
  answer<-tryCatch(suppressWarnings(reader()),error=function(e){problem<<-e;NULL})
  if(is.null(problem))return(answer)
  if(attempt<200L){Sys.sleep(.005);problem<-NULL}
 }
 stop(problem)
}
.rs_json <- function(path) {
 selected<-if(file.exists(path))path else if(file.exists(paste0(path,".previous")))paste0(path,".previous") else path
 if(!file.exists(selected))stop("项目文件不存在。",call.=FALSE)
 .rs_read_retry(function()jsonlite::read_json(selected,simplifyVector=FALSE))
}
.rs_read_rds <- function(path) .rs_read_retry(function()readRDS(path))
.rs_id <- function(prefix) paste0(prefix,"_",format(Sys.time(),"%Y%m%dT%H%M%OS6"),"_",Sys.getpid(),"_",substr(digest::digest(tempfile()),1,8))
.rs_project_validate <- function(p) {
 m<-.rs_json(file.path(p,"project.json"))
 if(!identical(m$project_schema_version,"1.0.0") || !identical(m$result_schema_version,"2.0.0")) stop("[PROJECT_SCHEMA] 不兼容的项目版本；请从原始计数重新分析，不会自动迁移。",call.=FALSE)
 if(!is.list(m$analyses)) stop("[PROJECT_SCHEMA] 分析历史格式不正确。")
 for(a in m$analyses) if(!is.character(a$analysis_id)||length(a$analysis_id)!=1 || !grepl("^[A-Za-z0-9_.-]+$",a$analysis_id)) stop("[PROJECT_SCHEMA] 非法分析标识。")
 list(path=normalizePath(p,winslash="/"),manifest=m)
}
rs_project_create <- function(name,root=getwd(),base=file.path(root,"user_projects")) {
 if(length(name)!=1 || !nzchar(trimws(name))) stop("请输入项目名称。")
 dir.create(base,recursive=TRUE,showWarnings=FALSE); base<-.rs_inside(base,.rs_workspace(root))
 p<-file.path(base,.rs_id("project")); dir.create(p)
 m<-list(project_schema_version="1.0.0",result_schema_version="2.0.0",view_schema_version="1.0.0",software_version=rs_release()$software_version,build_id=rs_release()$build_id,method_version="0.2.0",name=name,project_id=basename(p),created_utc=format(Sys.time(),tz="UTC",usetz=TRUE),analyses=list(),current_analysis_id=NULL)
 .rs_atomic(m,file.path(p,"project.json")); .rs_project_validate(p)
}
rs_project_open <- function(path,root=getwd()) {
 p<-.rs_inside(path,.rs_workspace(root)); ans<-.rs_project_validate(p);lock<-file.path(p,"task.lock")
 if(dir.exists(lock)) {
  owner<-tryCatch(.rs_json(file.path(lock,"owner.json")),error=function(e)NULL)
  if(!is.null(owner) && !.rs_pid_alive(owner$pid) && (is.null(owner$controller_pid)||!.rs_pid_alive(owner$controller_pid))) {
   d<-file.path(p,"analyses",owner$analysis_id);st<-tryCatch(.rs_json(file.path(d,"status.json")),error=function(e)list(status="failed"))
   if(file.exists(file.path(d,"cancel_requested.json"))){st$status<-"cancelled";st$stage<-"已取消";if(file.exists(file.path(d,"complete.json")))unlink(file.path(d,"complete.json"));.rs_atomic(st,file.path(d,"status.json"))}
   if(identical(st$status,"completed") && is.null(st$exit_code)){st$status<-"failed";st$stage<-"退出状态未核验";st$message<-"控制进程中断，无法核验最终退出码；生成文件保留，请重新运行。";.rs_atomic(st,file.path(d,"status.json"))}
   if(identical(st$status,"completed") && identical(st$exit_code,0L) && file.exists(file.path(d,"complete.json"))) {rs_project_result(ans,owner$analysis_id);ans$manifest$current_analysis_id<-owner$analysis_id;.rs_atomic(ans$manifest,file.path(p,"project.json"))}
   else if(!st$status%in%c("failed","cancelled")){st$status<-"failed";st$stage<-"进程中断";st$message<-"原任务已停止；旧结果保留，请重新运行。";.rs_atomic(st,file.path(d,"status.json"))}
   unlink(lock,recursive=TRUE)
  }
 }
 .rs_project_validate(p)
}
rs_project_list <- function(root=getwd()) {
 base<-file.path(root,"user_projects"); if(!dir.exists(base)) return(list())
 paths<-list.dirs(base,recursive=FALSE,full.names=TRUE)
 Filter(Negate(is.null),lapply(paths,function(p) tryCatch(rs_project_open(p,root),error=function(e) NULL)))
}
.rs_plain <- function(x,depth=0L) {
 if(depth>50L || is.environment(x)||is.function(x)||typeof(x)%in%c("externalptr","weakref","promise","language")) stop("[PROJECT_TYPE] 项目含不支持的对象类型。")
 if(is.list(x)) for(y in x) .rs_plain(y,depth+1L)
 invisible(TRUE)
}
rs_project_result <- function(project,analysis_id=NULL) {
 p<-.rs_project_path(project); m<-.rs_project_validate(p)$manifest
 if(is.null(analysis_id)) analysis_id<-m$current_analysis_id
 if(is.null(analysis_id) || !analysis_id %in% vapply(m$analyses,`[[`,character(1),"analysis_id")) stop("项目没有该分析结果。")
 d<-file.path(p,"analyses",analysis_id); marker<-file.path(d,"complete.json")
 if(file.exists(file.path(d,"cancel_requested.json")))stop("该分析已请求取消，不能作为成功结果打开。")
 if(!file.exists(marker)) stop("分析尚未完成，不能作为成功结果打开。")
 state<-.rs_json(file.path(d,"status.json"));if(!identical(state$status,"completed")||is.null(state$exit_code)||state$exit_code!=0L)stop("该分析最终退出状态尚未核验为成功；请查看诊断或重新运行。")
 completion<-.rs_json(marker); f<-file.path(d,"result.rds")
 if(!identical(completion$analysis_id,analysis_id)|| !identical(completion$sha256,digest::digest(file=f,algo="sha256"))) stop("结果标识或校验和不匹配。")
 x<-.rs_read_rds(f); .rs_plain(x)
 if(!identical(x$schema_version,"2.0.0")|| !identical(x$analysis_id,analysis_id)) stop("结果schema或分析标识不兼容。")
 x
}
.rs_snapshot <- function(project,counts_path,samples_path,config,view,figures,kind) {
 p<-.rs_project_path(project); m<-.rs_project_validate(p)$manifest; id<-.rs_id("analysis"); d<-file.path(p,"analyses",id); dir.create(d,recursive=TRUE)
 for(item in c("counts","samples")) {
  src<-get(paste0(item,"_path")); ext<-tolower(tools::file_ext(src)); if(!ext%in%c("csv","tsv","xlsx")) stop("输入仅支持CSV/TSV/XLSX。")
  dest<-file.path(d,paste0(item,".",ext)); if(!file.copy(src,dest)) stop("输入副本保存失败。")
 }
 if(is.character(config)) config<-yaml::read_yaml(config)
 yaml::write_yaml(config,file.path(d,"config.yml"))
 files<-c(paste0("counts.",tolower(tools::file_ext(counts_path))),paste0("samples.",tolower(tools::file_ext(samples_path))),"config.yml")
 snap<-list(analysis_id=id,kind=kind,created_utc=format(Sys.time(),tz="UTC",usetz=TRUE),software_version=rs_release()$software_version,build_id=rs_release()$build_id,method_version="0.2.0",schema_version="2.0.0",files=as.list(files),sha256=as.list(setNames(vapply(file.path(d,files),function(f) digest::digest(file=f,algo="sha256"),character(1)),files)),parent_analysis_id=m$current_analysis_id,exploratory_reanalysis=FALSE)
 if(!is.null(m$current_analysis_id)) {
  previous<-yaml::read_yaml(file.path(p,"analyses",m$current_analysis_id,"config.yml"))
  snap$raw_serialization_changed<-!identical(config,previous)
  effective_new<-tryCatch(.rs_config(config),error=function(e)NULL);effective_old<-tryCatch(.rs_config(previous),error=function(e)NULL)
  snap$configuration_changed<-is.null(effective_new)||is.null(effective_old)||!isTRUE(all.equal(effective_new,effective_old,tolerance=0))
  snap$exploratory_reanalysis<-identical(kind,"computed") && snap$configuration_changed
 } else {snap$configuration_changed<-FALSE;snap$raw_serialization_changed<-FALSE}
 .rs_atomic(snap,file.path(d,"snapshot.json")); .rs_atomic(list(view_schema_version="1.0.0",view=view,figures=figures),file.path(d,"view.json"))
 m$analyses<-c(m$analyses,list(list(analysis_id=id,kind=kind,created_utc=snap$created_utc))); .rs_atomic(m,file.path(p,"project.json")); list(id=id,path=d)
}
rs_project_start <- function(project,counts_path,samples_path,config,root=getwd(),view=list(),figures=list()) {
 p<-.rs_inside(.rs_project_path(project),.rs_workspace(root)); lock<-file.path(p,"task.lock")
 if(!dir.create(lock,showWarnings=FALSE)) stop("[TASK_BUSY] 本项目已有分析任务；请等待完成或取消。",call.=FALSE)
 success<-FALSE; on.exit(if(!success) unlink(lock,recursive=TRUE),add=TRUE)
 s<-.rs_snapshot(p,counts_path,samples_path,config,view,figures,"computed")
 .rs_atomic(list(analysis_id=s$id,status="running",stage="验证",started_utc=format(Sys.time(),tz="UTC",usetz=TRUE)),file.path(s$path,"status.json"))
 exe<-normalizePath(file.path(R.home("bin"),"Rscript.exe"),winslash="/",mustWork=TRUE)
 overrides<-c(R_LIBS_SITE=file.path(root,".no_site_library"),R_PROFILE_USER="NUL",R_ENVIRON_USER="NUL",R_LIBS=normalizePath(file.path(root,".library"),winslash="/"),R_LIBS_USER=normalizePath(file.path(root,".library"),winslash="/"),LC_ALL="English_United States.utf8",LC_CTYPE="English_United States.utf8",LANG="en_US.UTF-8",OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1")
 env<-Sys.getenv();env<-env[!names(env)%in%c("R_SESSION_TMPDIR","R_TESTS")];env[names(overrides)]<-overrides
 proc<-processx::process$new(exe,c("--vanilla",normalizePath(file.path(root,"scripts/analysis_worker.R"),winslash="/"),normalizePath(root,winslash="/"),p,s$id),wd=root,env=env,stdout=file.path(s$path,"worker.log"),stderr="2>&1",cleanup=FALSE,windows_hide_window=TRUE)
 .rs_atomic(list(pid=proc$get_pid(),controller_pid=Sys.getpid(),analysis_id=s$id),file.path(lock,"owner.json")); success<-TRUE
 list(project_path=p,analysis_id=s$id,process=proc,started=Sys.time(),state=new.env(parent=emptyenv()))
}
rs_task_poll <- function(task) {
 d<-file.path(task$project_path,"analyses",task$analysis_id)
 st<-tryCatch(.rs_json(file.path(d,"status.json")),error=function(e)list(status="running",stage="启动"))
 if(task$process$is_alive()) {
  if(st$status%in%c("result_ready","completed","failed","cancelled")){st$stage<-"等待进程退出核验";st$status<-"running"}
 } else {
  code<-task$process$get_exit_status();st$exit_code<-code
  if(file.exists(file.path(d,"cancel_requested.json"))){st$status<-"cancelled";st$stage<-"已取消";if(file.exists(file.path(d,"complete.json")))unlink(file.path(d,"complete.json"))}
  else if(st$status%in%c("result_ready","completed")&&identical(code,0L)){
   ready_file<-if(file.exists(file.path(d,"result_ready.json")))file.path(d,"result_ready.json")else file.path(d,"complete.json")
   verified<-tryCatch({ready<-.rs_json(ready_file);f<-file.path(d,"result.rds");x<-.rs_read_rds(f);.rs_plain(x);stopifnot(identical(ready$analysis_id,task$analysis_id),identical(ready$sha256,digest::digest(file=f,algo="sha256")),identical(x$schema_version,"2.0.0"),identical(x$analysis_id,task$analysis_id));ready$process_exit_code<-code;ready$exit_verified_by_controller<-Sys.getpid();.rs_atomic(ready,file.path(d,"complete.json"));TRUE},error=function(e){st$message<<-conditionMessage(e);FALSE})
   st$status<-if(verified)"completed"else"failed";st$stage<-if(verified)"完成"else"成果校验失败"
  } else if(!st$status%in%c("failed","cancelled")) {st$status<-"failed";st$stage<-"失败";st$message<-paste0("进程退出码=",code,"；生成文件保留作诊断，不作为成功分析。")}
  if(identical(st$status,"failed")&&file.exists(file.path(d,"complete.json"))){marker<-file.path(d,"complete.json");if(!file.copy(marker,file.path(d,"worker_completion_before_failure.json"),overwrite=TRUE))stop("无法保留失败完成标志证据。");unlink(marker)}
  .rs_atomic(st,file.path(d,"status.json"))
  owner<-tryCatch(.rs_json(file.path(task$project_path,"task.lock","owner.json")),error=function(e)NULL)
  owns_lock<-!is.null(owner)&&identical(owner$analysis_id,task$analysis_id)
  if(identical(st$status,"completed")&&owns_lock){m<-.rs_project_validate(task$project_path)$manifest;m$current_analysis_id<-task$analysis_id;.rs_atomic(m,file.path(task$project_path,"project.json"))}
  if(owns_lock)unlink(file.path(task$project_path,"task.lock"),recursive=TRUE)
 }
 st$elapsed_seconds<-as.numeric(difftime(Sys.time(),task$started,units="secs"));st$analysis_id<-task$analysis_id;st$project_path<-task$project_path;st
}
rs_task_cancel <- function(task) {
 d<-file.path(task$project_path,"analyses",task$analysis_id)
 if(!task$process$is_alive()) return(rs_task_poll(task))
 # Durable cancellation intent precedes termination, so worker completion cannot win a race.
 .rs_atomic(list(analysis_id=task$analysis_id,requested_utc=format(Sys.time(),tz="UTC",usetz=TRUE)),file.path(d,"cancel_requested.json"))
 handles<-tryCatch(ps::ps_children(ps::ps_handle(task$process$get_pid()),recursive=TRUE),error=function(e)list())
 root_handle<-tryCatch(ps::ps_handle(task$process$get_pid()),error=function(e)NULL)
 if(!is.null(root_handle)) handles<-c(handles,list(root_handle))
 tryCatch(task$process$kill_tree(),error=function(e){task$process$kill()})
 for(h in rev(handles))if(tryCatch(ps::ps_is_running(h),error=function(e)FALSE))try(ps::ps_kill(h,grace=0),silent=TRUE)
 task$process$wait(timeout=5000)
 alive<-vapply(handles,function(h)tryCatch(ps::ps_is_running(h),error=function(e)FALSE),logical(1))
 if(task$process$is_alive()||any(alive))stop("停止尚未完成，请保留窗口并稍后重试取消；当前任务仍被锁定。")
 if(file.exists(file.path(d,"complete.json")))unlink(file.path(d,"complete.json"))
 cancelled<-list(analysis_id=task$analysis_id,status="cancelled",stage="已取消",message="分析进程及已识别子进程已停止；旧结果保留。",terminated_pids=as.list(vapply(handles,ps::ps_pid,integer(1))),worker_exit_code=task$process$get_exit_status())
 .rs_atomic(cancelled,file.path(d,"status.json"))
 owner<-tryCatch(.rs_json(file.path(task$project_path,"task.lock","owner.json")),error=function(e)NULL)
 if(!is.null(owner)&&identical(owner$analysis_id,task$analysis_id))unlink(file.path(task$project_path,"task.lock"),recursive=TRUE)
 rs_task_poll(task)
}
rs_project_save_view <- function(project,view=list(),figures=list(),analysis_id=NULL) {
 p<-.rs_project_path(project);m<-.rs_project_validate(p)$manifest;if(is.null(analysis_id))analysis_id<-m$current_analysis_id
 if(is.null(analysis_id)||!analysis_id%in%vapply(m$analyses,`[[`,character(1),"analysis_id")) stop("请先选择已保存分析。")
 .rs_plain(view);.rs_plain(figures);.rs_atomic(list(view_schema_version="1.0.0",view=view,figures=figures),file.path(p,"analyses",analysis_id,"view.json"))
}
rs_project_add_result <- function(project,object,counts_path,samples_path,config=object$config,view=list(),figures=list()) {
 if(!identical(object$schema_version,"2.0.0")) stop("仅支持schema 2.0.0。")
 p<-.rs_project_path(project);if(dir.exists(file.path(p,"task.lock")))stop("项目正在计算。")
 s<-.rs_snapshot(project,counts_path,samples_path,config,view,figures,"opened_existing_result")
 object$analysis_id<-s$id;object<-rs_stamp_result(object); .rs_atomic(object,file.path(s$path,"result.rds"),TRUE)
 .rs_atomic(list(analysis_id=s$id,sha256=digest::digest(file=file.path(s$path,"result.rds"),algo="sha256")),file.path(s$path,"complete.json"))
 .rs_atomic(list(analysis_id=s$id,status="completed",stage="打开已存结果（未重新计算）",exit_code=0L),file.path(s$path,"status.json"))
 m<-.rs_project_validate(p)$manifest;m$current_analysis_id<-s$id;.rs_atomic(m,file.path(p,"project.json"));rs_project_open(p)
}
rs_project_export <- function(project,zipfile) {
 p<-.rs_project_path(project);.rs_project_validate(p);if(dir.exists(file.path(p,"task.lock")))stop("请在任务结束后导出项目包。")
 files<-list.files(p,recursive=TRUE,all.files=TRUE,no..=TRUE);files<-files[!grepl("(\\.tmp-|\\.previous$|worker\\.log$)",files)]
 zip::zipr(zipfile,files=files,root=p,include_directories=FALSE,mode="mirror");invisible(zipfile)
}
rs_project_import <- function(zipfile,root=getwd(),name=NULL) {
 z<-utils::unzip(zipfile,list=TRUE);n<-gsub("\\\\","/",z$Name)
 if(nrow(z)>10000 || sum(z$Length)>2e9 || any(z$Length>1e9)) stop("[PROJECT_ZIP_SIZE] 项目包超过允许大小。")
 if(any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)|:)",n))||anyDuplicated(tolower(n)))stop("[PROJECT_ZIP_PATH] 项目包包含越界或重复路径。")
 ext<-tolower(tools::file_ext(n)); if(any(!ext%in%c("json","rds","csv","tsv","xlsx","yml","yaml","txt","png","svg","pdf","html","log") & !grepl("/$",n)))stop("[PROJECT_ZIP_TYPE] 项目包包含不允许的文件类型；不执行包内脚本。")
 if(!"project.json"%in%n)stop("项目包缺少project.json。")
 base<-file.path(root,"user_projects");dir.create(base,recursive=TRUE,showWarnings=FALSE);p<-file.path(base,.rs_id("import"));dir.create(p)
 utils::unzip(zipfile,exdir=p); ans<-rs_project_open(p,root)
 for(a in ans$manifest$analyses) { .rs_snapshot_read(file.path(p,"analyses",a$analysis_id)); if(file.exists(file.path(p,"analyses",a$analysis_id,"complete.json"))){st<-.rs_json(file.path(p,"analyses",a$analysis_id,"status.json"));if(identical(st$status,"completed")&&!is.null(st$exit_code)&&st$exit_code==0L)rs_project_result(ans,a$analysis_id)} }
 if(!is.null(name)){ans$manifest$name<-name;.rs_atomic(ans$manifest,file.path(p,"project.json"))};rs_project_open(p,root)
}
rs_project_analysis_path <- function(project,analysis_id=NULL) {
 p<-.rs_project_path(project); m<-.rs_project_validate(p)$manifest;if(is.null(analysis_id))analysis_id<-m$current_analysis_id
 if(is.null(analysis_id)||!analysis_id%in%vapply(m$analyses,`[[`,character(1),'analysis_id'))stop('该项目不存在所选分析。')
 file.path(p,'analyses',analysis_id)
}
rs_project_inputs <- function(project,analysis_id=NULL) {
 d<-rs_project_analysis_path(project,analysis_id);snap<-.rs_snapshot_read(d);files<-unlist(snap$files)
 counts_path<-file.path(d,files[1]);samples_path<-file.path(d,files[2]);config<-yaml::read_yaml(file.path(d,'config.yml'))
 for(f in names(snap$sha256))if(!identical(snap$sha256[[f]],digest::digest(file=file.path(d,f),algo='sha256')))stop('保存的输入校验和不匹配。')
 x<-read_inputs(counts_path,samples_path,config);x$counts_path<-counts_path;x$samples_path<-samples_path;x
}

.rs_snapshot_read <- function(d) {
 snap<-.rs_json(file.path(d,"snapshot.json"));files<-unlist(snap$files)
 if(length(files)!=3L || !grepl("^counts[.](csv|tsv|xlsx)$",files[1]) || !grepl("^samples[.](csv|tsv|xlsx)$",files[2]) || files[3]!="config.yml" || !setequal(names(snap$sha256),files)) stop("[PROJECT_SNAPSHOT] 输入快照文件名称不合法。")
 snap
}

.rs_pid_alive <- function(pid) {
 if(length(pid)!=1L || !is.numeric(pid)||!is.finite(pid)||pid<1||pid%%1!=0)stop("Invalid worker PID")
 if(.Platform$OS.type=="windows") {
  q<-processx::run(file.path(Sys.getenv("SystemRoot"),"System32","tasklist.exe"),c("/FI",paste("PID eq",as.integer(pid)),"/FO","CSV","/NH"),error_on_status=TRUE)
  return(grepl(paste0('"',as.integer(pid),'"'),q$stdout,fixed=TRUE))
 }
 tools::pskill(pid,signal=0L)
}
