if(!exists('rs_release',mode='function'))source('R/000_release.R',encoding='UTF-8')
# Product views are derived from the immutable schema 2.0.0 object.
rs_class_keys <- function() c('within_reference','partial_reversal','overshoot','further_deviation','no_meaningful_change','insufficient_evidence','unconfirmed_perturbation','filtered','conflict')
rs_class_labels <- function(language='zh') stats::setNames(if(language=='zh') c('参考范围内恢复','部分回调','越过参考范围','进一步偏离','未发生有意义改变','分类证据不足','原始扰动未获支持','低表达过滤','声明冲突') else c('Within reference','Partial reversal','Overshoot','Further deviation','No meaningful change','Insufficient evidence','Perturbation unconfirmed','Filtered','Conflict'),rs_class_keys())
rs_palette <- function() stats::setNames(c('#0072B2','#009E73','#AA65A0','#C75B23','#C29400','#77808A','#BBC1C7','#E0E3E6','#332288'),rs_class_keys())
rs_short_label <- function(x) vapply(as.character(x),function(v)if(is.na(v))'NA' else if(nchar(v,type='width')<=24)v else paste0(substr(v,1,12),'...\n#',substr(digest::digest(v,algo='sha256',serialize=FALSE),1,6)),character(1),USE.NAMES=FALSE)
rs_available_fonts <- function() sort(unique(systemfonts::system_fonts()$family))
rs_style <- function(width=10,height=6,font_size=12,font_family=NULL,language='zh',point_size=1,label_ids=character(),label_n=0,palette=NULL) {
  fonts <- rs_available_fonts();preferred <- c('Microsoft YaHei','Noto Sans CJK SC','SimHei','Arial','sans')
  if(is.null(font_family)) font_family <- c(preferred[preferred %in% fonts],'sans')[1]
  if(!font_family %in% c(fonts,'sans','serif','mono')) stop('所选字体在本机不可用，请重新选择。',call.=FALSE)
  if(any(!is.finite(c(width,height,font_size,point_size,label_n))) || width<5 || width>30 || height<4 || height>30 || font_size<8 || font_size>28 || point_size<0.2 || point_size>5 || label_n<0 || label_n>20) stop('图宽高、字号、点大小或标签数量超出支持范围。',call.=FALSE)
  if(!language %in% c('zh','en')) stop('图标签语言须为 zh 或 en。',call.=FALSE)
  pal <- rs_palette();if(!is.null(palette)) {if(!all(names(palette)%in%names(pal)))stop('未知配色类别。');grDevices::col2rgb(palette);pal[names(palette)]<-palette}
  list(width=width,height=height,font_size=font_size,font_family=font_family,language=language,point_size=point_size,label_ids=as.character(label_ids),label_n=as.integer(label_n),palette=pal,label_rule='显式ID优先；否则按绝对t递减、gene_id升序固定选择；最多20个；静态图重叠文字可省略，全部选中ID保留在设置中',font_fallback='本机字体；不附字体文件，目标电脑需安装同名字体或自行替换')
}
rs_plot_data <- function(object,counts=NULL,samples=object$sample_mapping,analysis_id,intervention=NULL,gene_ids=NULL) {
  .rs_export_schema(object);z<-object$results;z$analysis_id<-as.character(analysis_id)
  # Per-contrast *_padj are descriptive BH fields, not primary BY class p-values.
  z<-z[,!names(z)%in%c('d_padj','t_padj','r_padj'),drop=FALSE];z$method<-'BY'
  z$class_key<-ifelse(is.na(z$classification)|!nzchar(z$classification),z$status,z$classification)
  if(is.null(intervention))intervention<-unique(z$intervention)[1]
  if(!intervention %in% z$intervention)stop('未找到所选干预。',call.=FALSE)
  map<-z[z$intervention==intervention & is.finite(z$d)&is.finite(z$t),,drop=FALSE]
  overview<-do.call(rbind,lapply(unique(z$intervention),function(a){zz<-z[z$intervention==a,,drop=FALSE];data.frame(intervention=a,class_key=rs_class_keys(),n=as.integer(table(factor(zz$class_key,levels=rs_class_keys()))),denominator=nrow(zz),analysis_id=analysis_id)}))
  if(is.null(gene_ids))gene_ids<-head(sort(unique(map$gene_id)),1)
  selected<-z[z$gene_id%in%gene_ids,,drop=FALSE]
  effects<-do.call(rbind,lapply(c('d','t','r'),function(k)data.frame(gene_id=selected$gene_id,intervention=selected$intervention,contrast=k,effect=selected[[k]],se=selected[[paste0(k,'_se')]],lower=selected[[paste0(k,'_lower')]],upper=selected[[paste0(k,'_upper')]],analysis_id=analysis_id)))
  ss<-data.frame(gene_id=character(),sample_id=character(),group=character(),role=character(),batch=character(),raw_count=numeric(),voom_logCPM=numeric(),analysis_id=character())
  if(!is.null(counts)) {
    if(is.data.frame(counts)&&'gene_id'%in%names(counts)){rownames(counts)<-counts$gene_id;counts<-as.matrix(counts[,setdiff(names(counts),'gene_id'),drop=FALSE])}
    for(g in intersect(gene_ids,rownames(counts))) {
      sm<-samples[match(colnames(counts),samples$sample_id),,drop=FALSE]
      e<-object$fit_inputs$expression;vals<-if(g%in%rownames(e))as.numeric(e[g,match(sm$sample_id,colnames(e))]) else rep(NA_real_,nrow(sm))
      ss<-rbind(ss,data.frame(gene_id=g,sample_id=sm$sample_id,group=sm$group,role=sm$role,batch=if('batch'%in%names(sm))as.character(sm$batch) else '未提供 / not supplied',raw_count=as.numeric(counts[g,]),voom_logCPM=vals,analysis_id=analysis_id))
    }
  }
  cb<-object$claims[,setdiff(names(object$claims),c('adjusted_p_BH','rejected_BH')),drop=FALSE];cb$analysis_id<-analysis_id;cb$method<-'BY'
  ch<-object$claims[,c('gene_id','intervention','class_name','direction','p_raw','adjusted_p_BH','rejected_BH','family_id'),drop=FALSE];ch$analysis_id<-analysis_id;ch$method<-'BH_exploratory';ch$view_note<-'Existing directional hypotheses only; no derived BH category'
  list(map=map,overview=overview,samples=ss,effects=effects,matrix=selected,claims_BY=cb,claims_BH=ch,direct=object$direct_comparisons,config=object$config,analysis_id=analysis_id,intervention=intervention)
}
rs_product_plot <- function(plot_data,type=c('map','overview','samples','effects','matrix'),style=rs_style()) {
  type<-match.arg(type);p<-plot_data;zh<-style$language=='zh';lab<-rs_class_labels(style$language);pal<-style$palette
  # Reuse the role ladder from figure-style; ggplot equivalent for export and preview.
  theme<-ggplot2::theme_minimal(base_size=style$font_size,base_family=style$font_family)+ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),panel.grid.major=ggplot2::element_line(colour='#EDF0F3'),axis.title=ggplot2::element_text(size=style$font_size),axis.text=ggplot2::element_text(size=style$font_size*.83),legend.text=ggplot2::element_text(size=style$font_size*.9),legend.title=ggplot2::element_blank(),plot.title=ggplot2::element_text(size=style$font_size,face='plain'),plot.caption=ggplot2::element_text(size=style$font_size*.83,hjust=0),legend.position='bottom',plot.margin=ggplot2::margin(12,18,12,12))
  colours<-ggplot2::scale_colour_manual(values=pal,limits=names(pal),labels=lab,drop=FALSE)
  if(type=='map') {
    z<-p$map;z$class_key<-factor(z$class_key,levels=names(pal));xx<-range(z$d);band<-data.frame(d=seq(xx[1],xx[2],length.out=100));band$lower<--band$d-p$config$epsilon_R;band$upper<--band$d+p$config$epsilon_R
    g<-ggplot2::ggplot(z,ggplot2::aes(d,t))+ggplot2::geom_ribbon(data=band,ggplot2::aes(x=d,ymin=lower,ymax=upper),inherit.aes=FALSE,fill='#DDEBF4',alpha=.55)+ggplot2::geom_hline(yintercept=0,linetype=2,colour='#62717D')+ggplot2::geom_abline(slope=-1,intercept=0,linewidth=.4)+ggplot2::geom_point(ggplot2::aes(colour=class_key,shape=class_key),size=style$point_size,alpha=.62)+colours+ggplot2::scale_shape_manual(values=c(16,17,15,18,8,3,1,4,7),limits=names(pal),labels=lab,drop=FALSE)+ggplot2::guides(colour=ggplot2::guide_legend(nrow=3,override.aes=list(alpha=1)),shape=ggplot2::guide_legend(nrow=3))+ggplot2::labs(title=paste(if(zh)'回调地图 ·' else 'Response map ·',p$intervention),x=if(zh)'扰动效应 d = D - C (log2)' else 'Perturbation d = D - C (log2)',y=if(zh)'干预效应 t = T - D (log2)' else 'Intervention t = T - D (log2)',caption=if(zh)paste0(nrow(z),' 个有效估计；主 BY。阴影是 |r| < ',p$config$epsilon_R,' 的参数范围，并非置信带。') else paste(nrow(z),'estimable rows; main BY. Shading: parameter range, not a confidence band.'))
    ids<-style$label_ids;if(!length(ids)&&style$label_n>0)ids<-head(z$gene_id[order(-abs(z$t),z$gene_id)],style$label_n)
    zz<-z[z$gene_id%in%head(ids,20),,drop=FALSE];if(nrow(zz))g<-g+ggplot2::geom_text(data=zz,ggplot2::aes(label=rs_short_label(gene_id)),family=style$font_family,size=style$font_size/3.5,vjust=-.7,check_overlap=TRUE)
  } else if(type=='overview') {
    z<-p$overview[p$overview$intervention==p$intervention,,drop=FALSE];z$class_key<-factor(z$class_key,levels=rev(names(pal)))
    g<-ggplot2::ggplot(z,ggplot2::aes(n,class_key,colour=class_key))+ggplot2::geom_segment(ggplot2::aes(x=0,xend=n,yend=class_key),linewidth=.6)+ggplot2::geom_point(size=2.5)+ggplot2::geom_text(ggplot2::aes(label=n),hjust=-.35,size=style$font_size/3.5,family=style$font_family,show.legend=FALSE)+colours+ggplot2::scale_y_discrete(labels=lab)+ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(.02,.18)))+ggplot2::labs(title=if(zh)'主 BY 类别与独立状态' else 'Main BY classes and distinct statuses',x=if(zh)'基因数（含输入中被过滤的基因）' else 'Genes (including filtered input genes)',y=NULL,caption=paste(if(zh)'分母：该干预全部输入基因' else 'Denominator: all input genes for this intervention',z$denominator[1]))+ggplot2::guides(colour='none')
  } else if(type=='samples') {
    z<-p$samples;z$group<-factor(z$group,levels=unique(z$group));z$x<-as.numeric(z$group)+ave(seq_len(nrow(z)),interaction(z$gene_id,z$group),FUN=function(x)seq(-.12,.12,length.out=length(x)))
    ns<-table(factor(z$group[z$gene_id==unique(z$gene_id)[1]],levels=levels(z$group)))
    g<-ggplot2::ggplot(z,ggplot2::aes(x,voom_logCPM,shape=batch))+ggplot2::geom_point(size=style$point_size+1,colour='#0072B2')+ggplot2::scale_x_continuous(breaks=seq_along(levels(z$group)),labels=paste0(levels(z$group),' (n=',ns,')'))+ggplot2::facet_wrap(~gene_id,scales='free_y')+ggplot2::labs(title=if(zh)'真实生物学样本点' else 'Observed biological samples',x=NULL,y='voom logCPM',caption=if(zh)'TMM/voom归一化展示值，未做批次校正；原始count在配套表中。模型效应单独展示。' else 'TMM/voom display values, not batch corrected; raw counts in the table. Model effects are separate.')
  } else if(type=='effects') {
    z<-p$effects
    g<-ggplot2::ggplot(z,ggplot2::aes(effect,contrast))+ggplot2::geom_vline(xintercept=0,colour='#77808A',linetype=2)+ggplot2::geom_segment(ggplot2::aes(x=lower,xend=upper,yend=contrast),linewidth=.6,colour='#0072B2')+ggplot2::geom_point(size=style$point_size+1,colour='#0072B2')+ggplot2::facet_grid(gene_id~intervention)+ggplot2::labs(title=if(zh)'统一模型效应与区间' else 'Unified model effects and intervals',x=if(zh)'效应（log2）' else 'Effect (log2)',y=NULL,caption=if(zh)'95%普通边际 moderated-t 区间；不是同时区间，也不是逐基因正确概率。' else '95% ordinary marginal moderated-t intervals; not simultaneous coverage.')
  } else {
    z<-p$matrix;z$label<-unname(lab[z$class_key]);if(nrow(z)>200)stop('矩阵图最多200个单元格；请显式选择较少基因，完整数据不受影响。',call.=FALSE)
    g<-ggplot2::ggplot(z,ggplot2::aes(intervention,gene_id,fill=class_key))+ggplot2::geom_tile(colour='white')+ggplot2::geom_text(ggplot2::aes(label=label),size=style$font_size/3.5,family=style$font_family)+ggplot2::scale_fill_manual(values=pal,labels=lab,drop=FALSE)+ggplot2::labs(title=if(zh)'同一基因集合的主 BY 状态' else 'Main BY status for the same genes',x=NULL,y=NULL,caption=if(zh)'声明状态不同不等于干预间存在显著差异；直接比较见独立检验族。' else 'Different claim status does not establish an intervention difference.')+ggplot2::guides(fill='none')
  }
  if(type=='overview') {
    g$layers[[3]]$aes_params$colour<-'#243746'
    g$layers[[3]]$position<-ggplot2::position_nudge(x=max(p$overview$n)*.015)
  }
  if(type=='samples' && identical(p$sample_scale,'count')) {
    g$mapping$y<-ggplot2::aes(y=raw_count)$y
    g<-g+ggplot2::labs(y=if(zh)'原始基因计数' else 'Raw gene counts',caption=if(zh)'原始count，未归一化、未做批次校正；文库大小可影响计数。模型效应单独展示。' else 'Raw counts, without normalization or batch correction; library size affects counts. Model effects are separate.')
  }
  if(type=='matrix') {
    rgb<-t(grDevices::col2rgb(pal[g$data$class_key]))/255
    lin<-ifelse(rgb<=.04045,rgb/12.92,((rgb+.055)/1.055)^2.4)
    lum<-as.numeric(lin%*%c(.2126,.7152,.0722))
    g$data$label_colour<-ifelse((lum+.05)/.05 >= 1.05/(lum+.05),'#111111','#FFFFFF')
    g$layers[[2]]$mapping$colour<-ggplot2::aes(colour=label_colour)$colour
    g<-g+ggplot2::scale_colour_identity()
  }
  if(type%in%c('samples','effects'))g$facet$params$labeller<-ggplot2::labeller(.default=rs_short_label)
  if(type=='matrix')g<-g+ggplot2::scale_x_discrete(labels=rs_short_label)+ggplot2::scale_y_discrete(labels=rs_short_label)
  g<-g+theme
  if(type=='map') {
    g$layers[[4]]$show.legend<-TRUE
    g<-g+ggplot2::labs(caption=if(zh)paste0(nrow(p$map),' 个有效估计；主 BY。虚线 t = 0；实线 t = -d。\n阴影为 |r| < ',p$config$epsilon_R,' 的参数范围，并非置信带。') else paste0(nrow(p$map),' estimable rows; main BY. Dashed: t = 0; solid: t = -d.\nShading: |r| < ',p$config$epsilon_R,' parameter range, not a confidence band.'))
  }
  ids_to_show<-c(if(type=='map')ids else unique(p$matrix$gene_id),p$intervention,if(type%in%c('samples','effects','matrix'))unique(p$matrix$intervention))
  if(any(nchar(ids_to_show,type='width')>24))g<-g+ggplot2::labs(caption=paste(g$labels$caption,if(zh)'长图标签已缩略并附固定ID摘要；完整ID见表，数据关联不变。' else 'Long display labels are shortened with a fixed ID hash; full IDs remain in the tables.',sep='\n'))
  if(type=='map')g<-g+ggplot2::labs(title=paste(if(zh)'回调地图 ·' else 'Response map ·',rs_short_label(p$intervention)))
  g
}
rs_save_plot <- function(plot,file,style=rs_style(),format=tools::file_ext(file)) {
  dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE)
  if(format=='svg')svglite::svglite(file,width=style$width,height=style$height,standalone=TRUE)
  else if(format=='pdf')grDevices::cairo_pdf(file,width=style$width,height=style$height,family=style$font_family)
  else if(format=='png')ragg::agg_png(file,width=style$width,height=style$height,units='in',res=300)
  else stop('支持 SVG、PDF 或 PNG。',call.=FALSE)
  on.exit(grDevices::dev.off());print(plot);invisible(file)
}
rs_write_table <- function(x,path) {
  utils::write.table(x,paste0(path,'.tsv'),sep='\t',quote=TRUE,row.names=FALSE,na='NA',fileEncoding='UTF-8')
  utils::write.table(x,paste0(path,'.csv'),sep=',',quote=TRUE,row.names=FALSE,na='NA',fileEncoding='UTF-8')
}
rs_html_table <- function(x,id=NULL) {
  esc<-function(x)as.character(htmltools::htmlEscape(as.character(x)))
  paste0('<table',if(!is.null(id))paste0(' id="',esc(id),'"') else '','><thead><tr>',paste0('<th>',esc(names(x)),'</th>',collapse=''),'</tr></thead><tbody>',paste(vapply(seq_len(nrow(x)),function(i)paste0('<tr>',paste0('<td>',esc(x[i,]),'</td>',collapse=''),'</tr>'),character(1)),collapse=''),'</tbody></table>')
}
rs_export_bundle <- function(object,counts,samples,analysis_id,out,style=rs_style(),intervention=NULL,gene_ids=NULL,filtered_keys=NULL,filter_description='未筛选',repo_root=getwd(),report_only=FALSE) {
  .rs_export_schema(object);dir.create(out,recursive=TRUE,showWarnings=FALSE);start<-proc.time()[3]
  p<-rs_plot_data(object,counts,samples,analysis_id,intervention,gene_ids)
  z<-object$results;z$analysis_id<-analysis_id;z$method<-'BY';z<-z[,!names(z)%in%c('d_padj','t_padj','r_padj'),drop=FALSE]
  key<-function(x)paste(nchar(x$gene_id),x$gene_id,nchar(x$intervention),x$intervention,sep=':')
  filtered<-if(is.null(filtered_keys))z else z[key(z)%in%key(filtered_keys),,drop=FALSE]
  candidates<-z[z$status=='classified' & z$classification%in%c('within_reference','partial_reversal'),,drop=FALSE]
  style_json<-style;style_json$palette<-as.list(style$palette)
  if(!report_only) {
  rs_write_table(z,file.path(out,'full_results_BY'));rs_write_table(filtered,file.path(out,'filtered_results_BY'));rs_write_table(candidates,file.path(out,'candidates_BY'))
  for(nm in c('map','overview','samples','effects','matrix','claims_BY','claims_BH','direct'))rs_write_table(p[[nm]],file.path(out,paste0('plot_data_',nm)))
  rs_write_table(object$families,file.path(out,'families'));rs_write_table(samples,file.path(out,'sample_mapping'))
  rs_write_table(data.frame(sample_id=rownames(object$design),object$design,check.names=FALSE),file.path(out,'design_matrix'))
  saveRDS(object,file.path(out,'result.rds'),version=3);yaml::write_yaml(object$config,file.path(out,'analysis.yml'));style_json<-style;style_json$palette<-as.list(style$palette);jsonlite::write_json(style_json,file.path(out,'figure_style.json'),auto_unbox=TRUE,pretty=TRUE)
  }
  if(is.data.frame(counts)&&'gene_id'%in%names(counts))ct<-counts else ct<-data.frame(gene_id=rownames(counts),counts,check.names=FALSE)
  if(!report_only){rs_write_table(ct,file.path(out,'counts'));rs_write_table(samples,file.path(out,'samples'))}
  types<-c('map','overview','samples','effects');if(length(unique(object$results$intervention))>1)types<-c(types,'matrix')
  for(ty in types){g<-rs_product_plot(p,ty,style);for(ext in if(report_only)'png' else c('svg','pdf','png'))rs_save_plot(g,file.path(out,paste0(ty,'.',ext)),style,ext)}
  if(!report_only) {
  src<-file.path(out,'reproduce');dir.create(src,showWarnings=FALSE);file.copy(file.path(repo_root,'R',c('core.R','export.R')),src,overwrite=TRUE);file.copy(file.path(repo_root,'renv.lock'),out,overwrite=TRUE)
  writeLines(c("# Run with the locked R/Bioconductor environment from this extracted directory.","args <- commandArgs(FALSE)","f <- sub('^--file=', '', args[grep('^--file=', args)])","if(length(f)) setwd(dirname(normalizePath(f)))","source('reproduce/core.R', encoding='UTF-8')","source('reproduce/export.R', encoding='UTF-8')","cfg <- yaml::read_yaml('analysis.yml')","x <- read_inputs('counts.tsv','samples.tsv',cfg)","o <- fit_response(x$counts,x$samples,x$config)","old <- readRDS('result.rds')","stopifnot(isTRUE(all.equal(o$results,old$results,tolerance=1e-8)))","saveRDS(o,'recomputed_result.rds')","cat('Recomputed from relative input paths; results agree.\\n')"),file.path(out,'rerun.R'),useBytes=TRUE)
  }
  manifest<-list(software_version=rs_release()$software_version,build_id=rs_release()$build_id,source_sha256=rs_release()$source_sha256,analysis_software_version=object$software_version,method_version=object$method_version,schema_version=object$schema_version,analysis_id=analysis_id,primary_method='BY',exploratory_method='BH existing claims only',filter_description=filter_description,full_rows=nrow(z),filtered_rows=nrow(filtered),candidate_rows=nrow(candidates),candidate_scope='Primary BY within_reference or partial_reversal over the complete analysis, not current filter',plot_scope=paste('Map: all estimable retained genes in',p$intervention,'; matrix/details: explicit selected gene IDs'),selected_genes=unique(p$matrix$gene_id),input_sha256=if(report_only)list(counts_R_serialization=digest::digest(counts,algo='sha256'),samples_R_serialization=digest::digest(samples,algo='sha256')) else list(counts=digest::digest(file=file.path(out,'counts.tsv'),algo='sha256'),samples=digest::digest(file=file.path(out,'samples.tsv'),algo='sha256')),canonical_sha256=if(report_only)digest::digest(object,algo='sha256') else digest::digest(file=file.path(out,'result.rds'),algo='sha256'),provenance=object$provenance,config=object$config,families=object$families,figure_style=style,elapsed_seconds=unname(proc.time()[3]-start))
  manifest$figure_style<-style_json
  jsonlite::write_json(manifest,file.path(out,'manifest.json'),auto_unbox=TRUE,pretty=TRUE,na='null')
  esc<-function(x)as.character(htmltools::htmlEscape(as.character(x)))
  image_html<-vapply(types,function(ty)paste0('<figure><img alt="',ty,'" src="data:image/png;base64,',base64enc::base64encode(file.path(out,paste0(ty,'.png'))),'"/><figcaption>',esc(ty),'</figcaption></figure>'),character(1))
  overview<-p$overview;overview$label<-unname(rs_class_labels()[overview$class_key])
  source_description <- if(is.null(object$config$source_description)) '' else as.character(object$config$source_description)
  case_limit <- if(grepl('GSE208041', source_description, fixed=TRUE)) '公开 GSE208041 是同一终点共同处理，29-mer及批次资料缺失限制保留，不解释为纵向治疗恢复。' else '按当前研究的实际处理时序、重复单位和已知协变量解释；转录组接近参考状态不等于治疗恢复。'
  html<-paste0('<!doctype html><html lang="zh"><meta charset="UTF-8"><meta name="viewport" content="width=device-width"><title>RevertScope 离线结果</title><style>body{font-family:"Microsoft YaHei",sans-serif;color:#243746;background:#f5f7fa;margin:30px auto;max-width:1100px;padding:20px}h1,h2{font-weight:500}section{background:white;padding:24px;margin:18px 0;border-radius:10px}table{border-collapse:collapse;width:100%;font-size:13px}td,th{padding:7px;border-bottom:1px solid #ddd;text-align:left;overflow-wrap:anywhere}img{max-width:100%;height:auto}.scroll{overflow:auto;max-height:520px}input{padding:10px;width:60%}code{overflow-wrap:anywhere}figure{margin:0}</style><h1>RevertScope 离线分析结果</h1><p>分析 ',esc(analysis_id),' · 软件 ',esc(rs_release()$software_version),' · 构建 ',esc(rs_release()$build_id),' · 方法 ',esc(object$method_version),' · schema ',esc(object$schema_version),'</p><p>此报告可离线查看，不支持重算。复算请在配套目录运行 Rscript rerun.R，使用锁定环境。</p><section><h2>输入与设计</h2><p>',nrow(ct),' 个输入基因；',nrow(samples),' 个生物学样本。当前干预：',esc(p$intervention),'</p><div class="scroll">',rs_html_table(samples),'</div></section><section><h2>参数与正式检验族</h2><pre>',esc(paste(capture.output(str(object$config)),collapse='\n')),'</pre><div class="scroll">',rs_html_table(object$families),'</div><p>主 BY 声明与 BH 探索性假设字段分别导出。筛选不会重新计算检验族；任意筛选子集不自动获得原检验族的同一 FDR 保证。</p></section><section><h2>结果概览</h2>',rs_html_table(overview),'<p>未获原始扰动支持不证明无扰动；分类证据不足不证明无干预作用；低表达过滤不表示无变化。类别为0表示本次没有支持的声明。窄等效范围及小样本可导致很低的检出能力。</p></section><section><h2>主要图表</h2>',paste(image_html,collapse=''),'<p>SVG保留可编辑文字与矢量元素；PDF保留矢量图形。字体 ',esc(style$font_family),' 来自本机，不随成果包分发，其他电脑需同名字体或手动替换。图中区间为95%普通边际区间；参数带不是置信带。</p></section><section><h2>候选查看</h2><p>全分析主 BY 的参考范围内恢复或部分回调，共 ',nrow(candidates),' 行；不是已验证靶点。</p><input id="search" placeholder="检索候选ID或干预" aria-label="检索候选"><div class="scroll">',rs_html_table(candidates[,intersect(c('gene_id','intervention','d','t','r','classification','classification_adjusted_p','analysis_id'),names(candidates)),drop=FALSE],'candidates'),'</div></section><section><h2>当前筛选及解释边界</h2><p>',esc(filter_description),'；筛选导出 ',nrow(filtered),' 行，全量导出 ',nrow(z),' 行。筛选表并非浏览器当前页。</p><p>干预声明状态不同不证明干预间差异；直接比较采用单独检验族。调整后P值不是逐基因错误概率。',esc(case_limit),'</p><p>数据来源：',esc(source_description),'</p></section><script>document.getElementById("search").addEventListener("input",function(){let q=this.value.toLowerCase();document.querySelectorAll("#candidates tbody tr").forEach(r=>r.hidden=!r.textContent.toLowerCase().includes(q))});</script></html>')
  details<-paste0('<section><h2>所选基因的模型效应与正式声明</h2><div class="scroll">',rs_html_table(p$effects),'</div><div class="scroll">',rs_html_table(p$matrix[,intersect(c('gene_id','intervention','classification','status','reason','classification_adjusted_p','analysis_id'),names(p$matrix)),drop=FALSE]),'</div><p>以上效应与状态来自规范结果对象。区间为95%普通边际区间；BY声明使用完整正式检验族。</p></section>')
  html<-sub('<section><h2>候选查看</h2>',paste0(details,'<section><h2>候选查看</h2>'),html,fixed=TRUE)
  if(nrow(p$direct)) {
    dt<-p$direct[p$direct$gene_id%in%unique(p$matrix$gene_id),,drop=FALSE]
    direct_html<-paste0('<section><h2>所选基因的干预间直接比较</h2><p>按 comparison 的实际方向解释；单独的 BY 检验族，不与类别声明合成分数。</p><div class="scroll">',rs_html_table(dt),'</div></section>')
    html<-sub('<section><h2>候选查看</h2>',paste0(direct_html,'<section><h2>候选查看</h2>'),html,fixed=TRUE)
  }
  html<-sub('code{overflow-wrap:anywhere}','code{overflow-wrap:anywhere}pre{white-space:pre-wrap;overflow-wrap:anywhere}',html,fixed=TRUE)
  # Preserve complete IDs and numeric strings; horizontal scrolling is clearer
  # than breaking an identifier or p-value across arbitrary lines.
  html<-sub('</style>','td,th{white-space:nowrap}.scroll{scrollbar-gutter:stable}</style>',html,fixed=TRUE)
  if(report_only)html<-sub('复算请在配套目录运行 Rscript rerun.R，使用锁定环境。','需要复算时，请另下载完整成果包，在解压目录运行 Rscript rerun.R，使用锁定环境。',html,fixed=TRUE)
  writeLines(html,file.path(out,'report.html'),useBytes=TRUE)
  writeLines(c('Export identity: see manifest.json. Main BY and exploratory BH are separate files.','Candidate table = complete-analysis primary BY within-reference/partial-reversal claims.','full_results_BY omits descriptive per-contrast BH-adjusted fields; canonical result.rds retains the exact schema.','SVG text is editable. Font files are not distributed. PDF uses Cairo vector graphics.','Offline report embeds PNG previews; SVG/PDF files are supplied separately.','Figure style and analysis configuration are independent. Rerun requires compatible locked dependencies.','Run Rscript rerun.R from this extracted folder; no F-drive paths are required.'),file.path(out,'EXPORT_README.txt'))
  manifest$elapsed_seconds<-unname(proc.time()[3]-start)
  jsonlite::write_json(manifest,file.path(out,'manifest.json'),auto_unbox=TRUE,pretty=TRUE,na='null')
  invisible(list(directory=out,html=file.path(out,'report.html'),manifest=manifest,plot_data=p))
}
rs_export_report <- function(object,counts,samples,analysis_id,file,style=rs_style(),intervention=NULL,gene_ids=NULL,filtered_keys=NULL,filter_description='未筛选',repo_root=getwd()) {
  staging<-tempfile('revertscope-report-');dir.create(staging)
  x<-rs_export_bundle(object,counts,samples,analysis_id,staging,style,intervention,gene_ids,filtered_keys,filter_description,repo_root,report_only=TRUE)
  dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE)
  if(!file.copy(x$html,file,overwrite=TRUE))stop('离线报告写入失败，请检查目标目录权限。',call.=FALSE)
  invisible(list(file=file,elapsed_seconds=x$manifest$elapsed_seconds,analysis_id=analysis_id))
}
