.libPaths(c(normalizePath('.library',mustWork=TRUE),.Library));Sys.setenv(R_LIBS_SITE=file.path(getwd(),'.no_site_library'))
args<-commandArgs(trailingOnly=TRUE)
port<-if('--port'%in%args)as.integer(args[match('--port',args)+1]) else 8765L
root<-normalizePath(getwd(),winslash='/');if(!file.exists(file.path(root,'R/core.R')))stop('从RevertScope项目根目录启动。')
browse<-('--browse'%in%args && !'--no-browser'%in%args)
cat(sprintf('APP_R_PID=%d\n',Sys.getpid()))
cat(sprintf('APP_LAUNCH_BROWSER=%s\n',browse))
cat(sprintf('RevertScope local application: http://127.0.0.1:%d\n',port))
shiny::runApp(file.path(root,'app'),host='127.0.0.1',port=port,launch.browser=browse,display.mode='normal')
