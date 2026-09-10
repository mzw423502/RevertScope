# Build identity is packaging metadata; the method implementation stays frozen.
rs_release <- function() {
 p<-getOption('revertscope.release_file','release_metadata.json')
 if(!file.exists(p))p<-system.file('release_metadata.json',package='revertscope')
 if(!nzchar(p)||!file.exists(p))stop('Missing release_metadata.json; reinstall the complete candidate.',call.=FALSE)
 jsonlite::read_json(p,simplifyVector=TRUE)
}
rs_stamp_result <- function(object) {
 m<-rs_release()
 if(is.null(object$analysis_software_version)&&!is.null(object$software_version))object$analysis_software_version<-object$software_version
 object$software_version<-m$software_version;object$build_id<-m$build_id
 object$software_source_sha256<-m$source_sha256
 object$diagnostics$versions$revertscope<-m$software_version
 object
}
