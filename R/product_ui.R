# Display-only decomposition of the existing directional IUT; never classifies.
rs_condition_evidence<-function(object,gene_id,intervention){
 z<-object$results[object$results$gene_id==gene_id & object$results$intervention==intervention,,drop=FALSE]
 if(nrow(z)!=1||!is.finite(z$d))return(data.frame())
 c<-object$config;rows<-list()
 for(s in c(1,-1)){
  add<-function(label,effect,se,bound,greater=TRUE){p<-if(greater)stats::pt((effect-bound)/se,z$df,lower.tail=FALSE)else stats::pt((effect-bound)/se,z$df);data.frame(direction=s,condition=label,effect=effect,bound=bound,raw_one_sided_p=p,unadjusted_condition_supported=p<=c$alpha)}
  rows[[length(rows)+1]]<-do.call(rbind,list(add('原扰动：s·d > δD',s*z$d,z$d_se,c$delta_D),add('改善：−s·t > δM',-s*z$t,z$t_se,c$delta_M),add('参考下界：r > −εR',z$r,z$r_se,-c$epsilon_R),add('参考上界：r < εR',z$r,z$r_se,c$epsilon_R,FALSE),add('同向残余：s·r > εR',s*z$r,z$r_se,c$epsilon_R),add('越过参考：s·r < −εR',s*z$r,z$r_se,-c$epsilon_R,FALSE),add('进一步偏离：s·t > δM',s*z$t,z$t_se,c$delta_M),add('无改变下界：t > −ε0',z$t,z$t_se,-c$epsilon_0),add('无改变上界：t < ε0',z$t,z$t_se,c$epsilon_0,FALSE)))
 }
 do.call(rbind,rows)
}
rs_input_error_detail <- function(error,uploads) {
 if(is.null(uploads)||is.null(error$code))return('')
 code<-error$code;raw<-uploads$ct
 if(code %in% c('COUNT_MISSING','NONINTEGER_COUNT','NEGATIVE_COUNT')) {
  x<-suppressWarnings(matrix(as.numeric(as.matrix(raw[-1L])),nrow=nrow(raw)))
  bad<-switch(code,COUNT_MISSING=!is.finite(x),NONINTEGER_COUNT=is.finite(x)&abs(x-round(x))>1e-8,NEGATIVE_COUNT=is.finite(x)&x<0)
  at<-head(which(bad,arr.ind=TRUE),8L)
  if(nrow(at))return(paste('具体位置（最多8条）：',paste(sprintf('gene_id=%s，样本=%s',raw[[1]][at[,1]],names(raw)[-1][at[,2]]),collapse='；')))
 }
 if(code %in% c('GENE_ID','DUPLICATE_ID')) {
  ids<-as.character(raw[[1]]);at<-which(is.na(ids)|!nzchar(trimws(ids))|duplicated(ids)|duplicated(ids,fromLast=TRUE))
  if(length(at))return(paste('计数表数据行：',paste(head(at,8),collapse=', '),'；请依据原始身份修正。'))
 }
 ''
}
