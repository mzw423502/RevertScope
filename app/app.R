library(shiny)
if(!file.exists('R/core.R') && file.exists('../R/core.R'))setwd('..')
source('R/core.R',encoding='UTF-8');source('R/export.R',encoding='UTF-8')
source('R/000_release.R',encoding='UTF-8')
source('R/projects.R',encoding='UTF-8');source('R/product_export.R',encoding='UTF-8')
source('R/product_ui.R',encoding='UTF-8')
root<-normalizePath(getwd(),winslash='/')
options(revertscope.release_file=file.path(root,'release_metadata.json'))
options(shiny.maxRequestSize=200*1024^2)
class_labels<-c(within_reference='参考范围内恢复',partial_reversal='部分回调',overshoot='越过参考范围',further_deviation='进一步偏离',no_meaningful_change='未发生有意义改变',insufficient_evidence='分类证据不足',unconfirmed_perturbation='原始扰动未获支持',filtered='低表达过滤')
pal<-rs_palette()
lbl<-function(z){a<-unname(class_labels[z]);a[is.na(a)]<-z[is.na(a)];a}
fmt<-function(x)ifelse(is.na(x),'—',formatC(x,digits=4,format='g'))
ui<-fluidPage(tags$head(tags$title('RevertScope · 转录组干预响应'),tags$link(rel='stylesheet',href='style.css'),tags$script(src='gene-selection.js')),
 div(class='topbar',div(class='brand','RevertScope',span('转录组干预响应')),
 div(class='topactions',actionButton('home','首页'),actionButton('new_analysis','新建分析',class='btn-primary'),actionButton('open_projects','打开项目'))),
 uiOutput('notice'),
 tabsetPanel(id='page',type='hidden',
 tabPanel('home',value='home',div(class='hero',div(class='eyebrow','本地分析 · 数据留在您的电脑'),h1('看清扰动后的干预响应'),p('从两张计数与样本表开始，查看完整效应、支持的响应类别，并带走可编辑成果。'),
 actionButton('home_new','新建分析',class='btn-primary btn-lg'),actionButton('home_open','打开已有项目',class='btn-lg')),
 div(class='examplegrid',div(class='card',span(class='pill','真实数据'),h3('LPS 与地塞米松共同处理'),p('打开 GSE208041 既有结果。保留共同处理、29-mer 及批次信息限制；结果查看不会重新计算。'),actionButton('example_public','查看真实示例')),
 div(class='card',span(class='pill','模拟数据'),h3('多个干预，共享参考与扰动'),p('查看固定万基因模拟的两个干预及直接比较。此示例用于操作，不新增正式性能证据。'),actionButton('example_multi','查看多干预示例'))),
 div(class='card',h3('三个步骤'),div(class='steps','1  导入与确认设计　 →　 2  运行并探索结果　 →　 3  保存和导出'))),
 tabPanel('import',value='import',h2('新建分析'),p(class='muted','导入两张表，确认实际实验设计后运行。不会自动取整、填补缺失或合并身份。'),
 fluidRow(column(6,div(class='card',h3('1 · 导入表格'),textInput('project_name','项目名称','我的转录组分析'),fileInput('counts_file','基因计数矩阵',accept=c('.csv','.tsv','.xlsx'),buttonLabel='选择文件',placeholder='CSV / TSV / XLSX'),fileInput('samples_file','样本信息表',accept=c('.csv','.tsv','.xlsx'),buttonLabel='选择文件',placeholder='至少包含 sample_id、group'),
 downloadButton('template_counts','计数模板'),downloadButton('template_samples','样本模板'),uiOutput('input_summary'),DT::DTOutput('counts_preview'),DT::DTOutput('samples_preview'))),
 column(6,div(class='card',h3('2 · 确认组别与来源'),selectInput('group_c','参考组 C',choices=character()),selectInput('group_d','扰动组 D',choices=character()),selectizeInput('group_t','D 背景下的干预组 T（可多选）',choices=character(),multiple=TRUE),uiOutput('role_summary'),
 checkboxInput('source_confirmed','我确认这是原始基因计数，不是 TPM / FPKM / log 表达量',FALSE),checkboxInput('independent_confirmed','我确认每个样本为独立生物学重复',FALSE),selectInput('design_type','实验设计',c('独立重复'='independent','配对设计（暂不支持）'='paired','纵向设计（暂不支持）'='longitudinal')),
 checkboxInput('use_batch','使用样本表中真实记录的 batch 固定批次',TRUE),
 tags$details(tags$summary('高级设置 · log2 阈值与统计说明'),p('默认主分析为完整类别与方向族的 BY 校正；BH 仅作独立的探索字段查看。默认阈值不是通用生物学标准。'),
 fluidRow(column(6,numericInput('delta_D','原始偏离 δD',.75,min=.01)),column(6,numericInput('epsilon_R','参考范围 εR',.5,min=.01))),fluidRow(column(6,numericInput('delta_M','最小改善 δM',.5,min=.01)),column(6,numericInput('epsilon_0','无实质改变 ε0',.25,min=.01))),p('要求 δD > εR > 0；δM > ε0 > 0。改参数需点击运行，创建新分析版本。')),
 actionButton('run_analysis','运行分析',class='btn-primary btn-lg'),uiOutput('import_error'))))),
 tabPanel('open',value='open',h2('打开项目'),div(class='card',selectInput('project_select','本机保存的项目',choices=character()),actionButton('open_selected','打开所选项目'),fileInput('project_file','或导入项目包（.zip）',accept='.zip',buttonLabel='选择项目包'),p('仅恢复保存的输入、结果与视图。包中脚本不会自动执行。'))),
 tabPanel('workspace',value='workspace',div(class='workspacehead',div(h2(textOutput('project_title')),div(id='active_analysis',textOutput('analysis_identity'))),div(actionButton('save_project','保存视图'),actionButton('rerun_saved','重新运行'),actionButton('edit_analysis','修改设计并新建版本'))),
 tags$details(tags$summary('已保存的分析历史'),uiOutput('analysis_history')),div(class='taskbar',uiOutput('task_status'),actionButton('cancel_analysis','取消当前任务')),
 tabsetPanel(id='worktab',
 tabPanel('响应总览',div(class='card',uiOutput('counts_cards'),p(class='quiet','小样本与窄等效范围可能难以获得类别证据。未确定不等于干预无效。')),
 fluidRow(column(3,div(class='card controls',selectInput('intervention','查看干预',choices=character()),selectInput('mode','结果视图',c('主分析 BY'='BY','探索性 BH 统计字段'='BH')),textInput('gene_search','搜索 gene_id'),actionButton('apply_search','搜索'),actionButton('reset_view','重置'),selectInput('class_filter','主 BY 类别 / 状态',c('全部'='all',setNames(names(class_labels),unname(class_labels)))),checkboxInput('selected_only','仅查看框选基因',FALSE),uiOutput('filter_note'),tags$details(tags$summary('图形样式（不重新拟合）'),selectInput('style_language','图标签语言',c('中文'='zh','English'='en')),selectInput('style_palette','配色',c('标准分类色'='standard','柔和分类色'='muted')),selectInput('style_font','字体',c('Microsoft YaHei','Arial')),numericInput('style_size','字号',12,min=8,max=24),numericInput('style_width','导出宽度（英寸）',10,min=5,max=20),numericInput('style_height','导出高度（英寸）',6,min=4,max=18),numericInput('style_point','点大小',1,min=.3,max=4,step=.1),numericInput('style_labels','自动标签数量（|t|递减、gene_id打破并列）',0,min=0,max=20),textInput('manual_labels','手动标签 gene_id（逗号分隔）')))),
 column(9,div(class='card',uiOutput('map_frame'),p(class='quiet','横轴 d = D−C；纵轴 t = T−D。斜带是 |r| < εR 的参数范围，不是置信带。点落在带内不自动获得恢复声明。超长图标签缩写，完整ID见悬停和表格。'),uiOutput('mode_note')))),
 div(class='card',h3('结果表'),checkboxInput('advanced_columns','展开标准误、区间与状态字段',FALSE),DT::DTOutput('results_table'),p(class='quiet','显示与框选仅用于探索；不会重算检验族，也不为任意筛选子集提供新的 FDR 保证。'))),
 tabPanel('基因详情',div(class='card',selectizeInput('gene_pick','选择基因 gene_id',choices=character()),uiOutput('gene_explanation'),uiOutput('gene_identity')),
 fluidRow(column(6,div(class='card',radioButtons('expression_scale','样本显示',c('原始 count'='count','模型输入 logCPM（TMM/voom）'='logCPM'),inline=TRUE),plotly::plotlyOutput('sample_plot',height='360px'))),column(6,div(class='card',plotly::plotlyOutput('effect_plot',height='360px'),p('95% 普通边际 moderated-t 区间；有批次时模型效应已调整，样本图没有额外批次校正。')))),
 div(class='card',h3('效应、标准误与区间'),DT::DTOutput('effect_values'),h3('类别条件与证据'),DT::DTOutput('gene_claims'),uiOutput('condition_details'),tags$details(tags$summary('查看每个必要条件的证据'),p('下表仅拆解未校正单侧条件；单个条件通过不等于正式类别。类别仍以完整族 BY 声明为准。'),DT::DTOutput('condition_table')))),
 tabPanel('干预比较',div(class='card',uiOutput('comparison_note'),plotly::plotlyOutput('matrix_plot',height='330px'),DT::DTOutput('direct_table'),p(class='quiet','不同干预的证据状态不同，不等于“A特异有效”。直接比较另族 BY，不合并为综合评分。'))),
 tabPanel('导出',div(class='card',h3('带走完整成果'),p('图形使用当前样式，统计结果保持不变。地图导出包括当前干预的全部有效估计，筛选仅改变筛选表；不把当前浏览器页当成全量。SVG文字与图形为矢量；目标电脑仍需安装相应字体。离线报告仅查看结果，不支持重新计算。'),
 div(class='downloadgrid',downloadButton('dl_full_tsv','全量结果 TSV'),downloadButton('dl_full_csv','全量结果 CSV'),downloadButton('dl_filtered','当前筛选 TSV'),downloadButton('dl_candidates','主 BY 候选表'),downloadButton('dl_bh','探索 BH 声明统计 TSV'),downloadButton('dl_svg','回调地图 SVG'),downloadButton('dl_pdf','回调地图 PDF'),downloadButton('dl_html','离线 HTML 报告'),downloadButton('dl_bundle','全部成果包'),downloadButton('dl_project','可重开项目包')),
 uiOutput('export_note')))),
 tags$details(class='card diagnostic',tags$summary('诊断与版本详情'),verbatimTextOutput('diagnostics')))),
 tags$footer(paste('RevertScope · 本地软件',rs_release()$software_version,'· 方法',rs_release()$method_version,'· build',rs_release()$build_id,'· 仅支持独立生物学重复的 bulk RNA-seq')))

server<-function(input,output,session){
 rv<-reactiveValues(project=NULL,obj=NULL,counts=NULL,samples=NULL,task=NULL,last_task=NULL,error=NULL,notice=NULL,uploads=NULL,base_config=NULL,search='',selected=character(),gene=NULL,bundles=list(),tick=0,selection_epoch=0L,selection_revision=0L,selection_client_seq=0L,gene_labels=NULL,gene_url=NULL)
 tmp<-tempfile('rs_session_');dir.create(tmp)
 # Selection is UI state only; never launch a statistical task from this path.
 select_gene<-function(gene,arm=NULL,client_seq=NULL){
  if(is.null(rv$obj)||length(gene)!=1L||!gene%in%rv$obj$results$gene_id)return(invisible(FALSE))
  if(is.null(arm))arm<-isolate(input$intervention)
  if(length(arm)!=1L||!arm%in%rv$obj$results$intervention)arm<-rv$obj$results$intervention[1]
  if(!is.null(client_seq)){
   if(client_seq<rv$selection_client_seq)return(invisible(FALSE))
   rv$selection_client_seq<-client_seq
  }
  rv$gene<-gene;rv$selection_revision<-rv$selection_revision+1L
  label<-unname(rv$gene_labels[gene]);if(length(label)!=1L||is.na(label))label<-gene
  session$sendCustomMessage('rs_gene_selection',list(gene_id=gene,label=label,
   analysis_id=rv$obj$analysis_id,intervention=arm,epoch=rv$selection_epoch,
   revision=rv$selection_revision,client_seq=rv$selection_client_seq,url=rv$gene_url))
  invisible(TRUE)
 }
 configure_genes<-function(obj,genes){
  labels<-genes
  if('gene_symbol'%in%names(obj$results)){
   symbols<-as.character(obj$results$gene_symbol[match(genes,obj$results$gene_id)])
   valid<-!is.na(symbols)&nzchar(trimws(symbols))&symbols!=genes
   labels[valid]<-paste(symbols[valid],genes[valid],sep=' · ')
  }
  rv$gene_labels<-setNames(labels,genes);rv$selection_epoch<-rv$selection_epoch+1L
  rv$selection_client_seq<-0L
  choices<-data.frame(value=genes,label=labels,stringsAsFactors=FALSE)
  rv$gene_url<-session$registerDataObj(paste0('gene_options_',rv$selection_epoch),choices,function(data,req){
   q<-shiny::parseQueryString(req$QUERY_STRING)$query;if(is.null(q))q<-''
   keep<-if(nzchar(q))which(grepl(q,data$value,fixed=TRUE)|grepl(q,data$label,fixed=TRUE))else seq_len(nrow(data))
   exact<-which(data$value==q);keep<-head(unique(c(exact,keep)),100L)
   shiny::httpResponse(200L,'application/json; charset=UTF-8',jsonlite::toJSON(data[keep,,drop=FALSE],dataframe='rows',auto_unbox=TRUE))
  })
 }

 alive<-function()!is.null(rv$task)&&isTRUE(rv$task$process$is_alive())
 inform<-function(x){rv$notice<-x}
 handle<-function(expr)tryCatch(expr,error=function(e){rv$error<-conditionMessage(e);inform(paste('请修正后重试：',conditionMessage(e)));NULL})
 output$notice<-renderUI(if(!is.null(rv$notice))div(class='notice',rv$notice))
 output$import_error<-renderUI(if(!is.null(rv$error))div(class='errorbox',rv$error))
 go<-function(page)updateTabsetPanel(session,'page',selected=page)
 observeEvent(input$home,go('home'));observeEvent(input$home_open,{refresh_projects();go('open')});observeEvent(input$open_projects,{refresh_projects();go('open')})
 new<-function(){rv$base_config<-NULL;rv$notice<-NULL;rv$project<-NULL;rv$obj<-NULL;rv$counts<-NULL;rv$samples<-NULL;rv$error<-NULL;go('import')}
 observeEvent(input$new_analysis,new());observeEvent(input$home_new,new())
 refresh_projects<-function(){p<-rs_project_list(root);if(is.data.frame(p)){choices<-setNames(p$path,p$name)}else choices<-setNames(vapply(p,function(x)x$path,character(1)),vapply(p,function(x)x$manifest$name,character(1)));updateSelectInput(session,'project_select',choices=choices)}
 load_project<-function(p,id=NULL){
  pr<-rs_project_open(.rs_project_path(p),root);obj<-rs_project_result(pr,id);ins<-rs_project_inputs(pr,id)
  rv$project<-pr;rv$obj<-obj;rv$counts<-ins$counts;rv$samples<-ins$samples;rv$error<-NULL;rv$search<-'';rv$selected<-character();rv$bundles<-list()
  arms<-unique(obj$results$intervention);updateSelectInput(session,'intervention',choices=arms,selected=arms[1]);genes<-unique(obj$results$gene_id[is.finite(obj$results$d)])
  configure_genes(obj,genes);select_gene(genes[1],arms[1]);updateTextInput(session,'gene_search',value='');updateSelectInput(session,'class_filter',selected='all');go('workspace')
  vp<-file.path(rs_project_analysis_path(pr,obj$analysis_id),'view.json')
  if(file.exists(vp)){saved<-jsonlite::read_json(vp);v<-saved$view;f<-saved$figures
   if(!is.null(v$intervention)&&v$intervention%in%arms)updateSelectInput(session,'intervention',selected=v$intervention)
   if(!is.null(v$gene_id)&&v$gene_id%in%genes){select_gene(v$gene_id,if(!is.null(v$intervention)&&v$intervention%in%arms)v$intervention else arms[1])}
   if(!is.null(v$search)){rv$search<-v$search;updateTextInput(session,'gene_search',value=v$search)}
   if(!is.null(v$class_filter))updateSelectInput(session,'class_filter',selected=v$class_filter)
   if(!is.null(v$palette_name))updateSelectInput(session,'style_palette',selected=v$palette_name)
   if(!is.null(v$mode))updateSelectInput(session,'mode',selected=v$mode)
   for(k in c(width='style_width',height='style_height',font_size='style_size',point_size='style_point',label_n='style_labels')){nm<-names(c(width='style_width',height='style_height',font_size='style_size',point_size='style_point',label_n='style_labels'))[match(k,c(width='style_width',height='style_height',font_size='style_size',point_size='style_point',label_n='style_labels'))];if(!is.null(f[[nm]]))updateNumericInput(session,k,value=f[[nm]])}
   if(!is.null(f$font_family))updateSelectInput(session,'style_font',selected=f$font_family)
   if(!is.null(f$language))updateSelectInput(session,'style_language',selected=f$language)
   if(length(f$label_ids))updateTextInput(session,'manual_labels',value=paste(unlist(f$label_ids),collapse=','))
  }
 }
 observeEvent(input$open_selected,handle({req(input$project_select);load_project(input$project_select);inform('已打开保存的结果，没有重新计算。')}))
 observeEvent(input$project_file,handle({p<-rs_project_import(input$project_file$datapath,root);load_project(p);inform('项目包已安全导入，恢复保存的结果。')}))
 example<-function(which){handle({
  base<-file.path(root,'examples/projects',which);if(!file.exists(file.path(base,'project.json')))base<-file.path(root,'results/round3/builtin',which);if(!file.exists(file.path(base,'project.json')))stop(if(which=='public')'真实案例尚未恢复。请先运行 python scripts/fetch_release_public.py 联网获取官方文件，再运行 scripts/r.ps1 scripts/prepare_release_examples.R 生成案例项目。'else '模拟示例项目缺失，请重新安装完整软件包。',call.=FALSE);p<-rs_project_open(base,root);load_project(p);inform(if(which=='public')'真实数据：同一终点共同处理，29-mer；无可用批次元数据。' else '固定多干预模拟：操作示例，不新增正式基准证据。')})}
 observeEvent(input$example_public,example('public'));observeEvent(input$example_multi,example('multi'))
 observeEvent(list(input$counts_file,input$samples_file),{
  if(is.null(input$counts_file)||is.null(input$samples_file))return()
  handle({for(nm in c('counts','samples')){f<-input[[paste0(nm,'_file')]];ext<-tolower(tools::file_ext(f$name));if(!ext%in%c('csv','tsv','xlsx'))stop('仅支持 CSV、TSV、XLSX。');file.copy(f$datapath,file.path(tmp,paste0(nm,'.',ext)),overwrite=TRUE)}
   cp<-file.path(tmp,paste0('counts.',tools::file_ext(input$counts_file$name)));sp<-file.path(tmp,paste0('samples.',tools::file_ext(input$samples_file$name)))
   ct<-.rs_read_table(cp);sm<-.rs_read_table(sp);if(!all(c('sample_id','group')%in%names(sm)))stop('样本表需要 sample_id 与 group 列；请使用模板。')
   rv$base_config<-NULL;rv$uploads<-list(counts_path=cp,samples_path=sp,ct=ct,sm=sm);updateCheckboxInput(session,'source_confirmed',value=FALSE);updateCheckboxInput(session,'independent_confirmed',value=FALSE);g<-unique(sm$group);role<-if('role'%in%names(sm))sm$role else rep('',nrow(sm));sel<-function(r,default){x<-unique(sm$group[role==r]);if(length(x))x else default}
   updateSelectInput(session,'group_c',choices=g,selected=sel('C',g[1]));updateSelectInput(session,'group_d',choices=g,selected=sel('D',g[min(2,length(g))]));updateSelectizeInput(session,'group_t',choices=g,selected=sel('T',g[seq_along(g)>2]));rv$error<-NULL
  })
 })
 output$input_summary<-renderUI({req(rv$uploads);u<-rv$uploads;div(p(sprintf('计数矩阵：%s 个基因 × %s 个样本；样本表：%s 行。',nrow(u$ct),ncol(u$ct)-1,nrow(u$sm))),p('下面仅为前5条预览；完整数据将在运行前由内核验证。'))})
 output$counts_preview<-DT::renderDT({req(rv$uploads);DT::datatable(head(rv$uploads$ct[,seq_len(min(6,ncol(rv$uploads$ct))),drop=FALSE],5),options=list(dom='t',scrollX=TRUE),rownames=FALSE)})
 output$samples_preview<-DT::renderDT({req(rv$uploads);DT::datatable(head(rv$uploads$sm,5),options=list(dom='t',scrollX=TRUE),rownames=FALSE)})
 output$role_summary<-renderUI({req(rv$uploads);s<-rv$uploads$sm;g<-unique(s$group);tags$ul(lapply(g,function(x)tags$li(paste(x,':',sum(s$group==x),'个样本；',if(x==input$group_c)'参考 C' else if(x==input$group_d)'扰动 D' else if(x%in%input$group_t)'干预 T' else '未指定角色'))))})
 config_now<-function(){cfg<-if(is.null(rv$base_config))yaml::read_yaml(file.path(root,'config/analysis.yml'))else rv$base_config;cfg$source_confirmed<-isTRUE(input$source_confirmed);if(is.null(rv$base_config))cfg$source_description<-'用户在本地界面确认原始基因计数来源';cfg$independent_replicates<-isTRUE(input$independent_confirmed);cfg$design_type<-input$design_type;for(k in c('delta_D','epsilon_R','delta_M','epsilon_0'))cfg[[k]]<-input[[k]];cfg}
 friendly<-function(e){code<-if(!is.null(e$code))e$code else '';msg<-switch(code,SOURCE_CONFIRMATION='请确认原始基因计数来源；TPM、FPKM 或 log 表达不能取整后使用。',UNSUPPORTED_DESIGN='本版仅支持独立生物学重复；配对、纵向或单细胞设计不能按独立样本运行。',REPLICATES='存在组别的独立生物学重复少于3个，请核对实验记录。',THRESHOLD_ORDER='阈值组合不合法：必须 δD > εR > 0，且 δM > ε0 > 0。',DESIGN_NOT_ESTIMABLE='组别与批次混杂，设计不可估计；需要跨批次组别重叠，不能为出结果删掉真实批次。',COUNT_MISSING='存在缺失或非数字计数，请回到源文件修正。',NONINTEGER_COUNT='存在非整数计数，请提供原始基因计数，不能自动取整。',SAMPLE_MAPPING='样本对应失败：计数列名必须与 sample_id 一一匹配。',GENE_ID='gene_id 缺失或重复，请确认身份后修正。',conditionMessage(e));paste(msg,conditionMessage(e),rs_input_error_detail(e,rv$uploads))}
 start_task<-function(p,cp,sp,cfg){
  if(alive()){inform('已有分析正在运行，请等待或取消；没有重复排队。');return()}
  task<-rs_project_start(p,cp,sp,cfg,root,view=view(),figures=style());rv$task<-task;rv$last_task<-NULL;rv$project<-rs_project_open(.rs_project_path(p),root);rv$error<-NULL;inform('分析已启动。显示设置不会改变正在运行的输入快照。');go('workspace')
 }
 observeEvent(input$run_analysis,{
  if(alive()){inform('已有任务运行，没有重复排队。');return()}
  tryCatch({req(rv$uploads);u<-rv$uploads;cfg<-config_now();.rs_config(cfg);s<-u$sm
   if(input$group_c==input$group_d||any(input$group_t%in%c(input$group_c,input$group_d)))stop('C、D、T 必须是不同的实际组别。')
   s$role<-ifelse(s$group==input$group_c,'C',ifelse(s$group==input$group_d,'D',ifelse(s$group%in%input$group_t,'T','未指定')))
   if(any(s$role=='未指定'))stop('每个样本必须明确分配角色；不会静默删除未选组别。')
   if(!isTRUE(input$use_batch)&&'batch'%in%names(s)&&length(unique(s$batch))>1)stop('样本表存在多个真实批次，请保留固定批次；不为产生结果忽略批次。')
   sp<-file.path(tmp,'mapped_samples.tsv');write.table(s,sp,sep='\t',quote=FALSE,row.names=FALSE)
   read_inputs(u$counts_path,sp,cfg)
   p<-if(is.null(rv$project))rs_project_create(input$project_name,root) else rv$project
   start_task(p,u$counts_path,sp,cfg)
  },error=function(e){rv$error<-friendly(e);inform(rv$error)})
 })
 observe({invalidateLater(700,session);if(is.null(rv$task))return();task<-rv$task;st<-rs_task_poll(task);rv$last_task<-st
  if(st$status%in%c('completed','success','failed','cancelled')){rv$task<-NULL;if(st$status%in%c('completed','success')){current<-if(is.null(rv$project))'' else rv$project$path;if(identical(normalizePath(current,winslash='/',mustWork=FALSE),normalizePath(task$project_path,winslash='/',mustWork=FALSE))){handle(load_project(current,task$analysis_id));inform('分析完成；结果已保存为新版本。')}else inform('先前项目的任务已完成并保存，没有替换当前项目。')}else inform(paste('任务',st$status,st$message));}
 })
 observeEvent(input$cancel_analysis,{if(!is.null(rv$task)){handle(rs_task_cancel(rv$task));inform('已请求停止当前子进程；旧结果保留。')}})
 output$task_status<-renderUI({st<-rv$last_task;if(alive())div(strong('正在分析 · '),if(is.null(st))'验证输入' else paste(st$stage,st$elapsed_seconds,'秒'))else if(!is.null(st))div('最近任务：',st$status)else div('已有结果可查看；重新运行才创建新分析。')})
 output$project_title<-renderText(if(is.null(rv$project))'分析工作台' else rv$project$manifest$name)
 output$analysis_identity<-renderText({if(is.null(rv$obj))return('等待本次分析完成');snap<-jsonlite::read_json(file.path(rs_project_analysis_path(rv$project,rv$obj$analysis_id),'snapshot.json'));paste('分析版本',rv$obj$analysis_id,' · 主 BY · 方法',rv$obj$method_version,if(isTRUE(snap$exploratory_reanalysis))' · 修改配置后的探索性新分析'else if(identical(snap$kind,'opened_existing_result'))' · 打开已存结果，未重新计算'else ' · 已实际计算')})
 output$analysis_history<-renderUI({req(rv$project);tags$ul(lapply(rv$project$manifest$analyses,function(a)tags$li(paste(a$analysis_id,a$created_utc,if(a$kind=='computed')'运行记录（状态见诊断或项目）'else '已有结果'))))})
 observeEvent(input$rerun_saved,handle({req(rv$project,rv$obj);ins<-rs_project_inputs(rv$project,rv$obj$analysis_id);cp<-file.path(tmp,'rerun_counts.tsv');sp<-file.path(tmp,'rerun_samples.tsv');write.table(data.frame(gene_id=rownames(ins$counts),ins$counts,check.names=FALSE),cp,sep='\t',quote=FALSE,row.names=FALSE);write.table(ins$samples,sp,sep='\t',quote=FALSE,row.names=FALSE);start_task(rv$project,cp,sp,ins$config)}))
 observeEvent(input$edit_analysis,handle({req(rv$project);ins<-rs_project_inputs(rv$project);rv$base_config<-ins$config;updateSelectInput(session,'design_type',selected=ins$config$design_type);updateCheckboxInput(session,'use_batch',value=TRUE);cp<-file.path(tmp,'edit_counts.tsv');sp<-file.path(tmp,'edit_samples.tsv');write.table(data.frame(gene_id=rownames(ins$counts),ins$counts,check.names=FALSE),cp,sep='\t',quote=FALSE,row.names=FALSE);write.table(ins$samples,sp,sep='\t',quote=FALSE,row.names=FALSE);rv$uploads<-list(counts_path=cp,samples_path=sp,ct=.rs_read_table(cp),sm=ins$samples);s<-ins$samples;g<-unique(s$group);updateSelectInput(session,'group_c',choices=g,selected=unique(s$group[s$role=='C']));updateSelectInput(session,'group_d',choices=g,selected=unique(s$group[s$role=='D']));updateSelectizeInput(session,'group_t',choices=g,selected=unique(s$group[s$role=='T']));updateCheckboxInput(session,'source_confirmed',value=TRUE);updateCheckboxInput(session,'independent_confirmed',value=TRUE);for(k in c('delta_D','epsilon_R','delta_M','epsilon_0'))updateNumericInput(session,k,value=ins$config[[k]]);go('import');inform('修改后点击运行，将建立新的分析版本；旧结果不变。')}))
 style<-reactive(rs_style(palette=if(identical(input$style_palette,'muted'))setNames(c('#347C80','#709253','#A66B99','#BC7660','#6B8299','#9B9B9B','#B4B4B4','#D7D7D7','#794E87'),names(rs_palette()))else rs_palette(),width=input$style_width,height=input$style_height,font_size=input$style_size,font_family=input$style_font,language=input$style_language,point_size=input$style_point,label_n=input$style_labels,label_ids=trimws(strsplit(input$manual_labels,',',fixed=TRUE)[[1]])))
 view<-function()list(intervention=input$intervention,search=rv$search,class_filter=input$class_filter,mode=input$mode,gene_id=rv$gene,palette_name=input$style_palette)
 observeEvent(input$save_project,handle({req(rv$project);rs_project_save_view(rv$project,view(),style(),rv$obj$analysis_id);inform('项目视图与图形设置已保存；统计结果未改变。')}))
 full<-reactive({req(rv$obj,input$intervention,input$intervention%in%unique(rv$obj$results$intervention));z<-rv$obj$results[rv$obj$results$intervention==input$intervention,,drop=FALSE];z$display_state<-ifelse(is.na(z$classification),z$status,z$classification);z$key<-paste(rv$obj$analysis_id,z$intervention,z$gene_id,sep='␟');z})
 filtered<-reactive({z<-full();if(nzchar(rv$search))z<-z[grepl(rv$search,z$gene_id,fixed=TRUE),,drop=FALSE];if(input$class_filter!='all')z<-z[z$display_state==input$class_filter,,drop=FALSE];if(isTRUE(input$selected_only))z<-z[z$key%in%rv$selected,,drop=FALSE];z})
 observeEvent(input$apply_search,{rv$search<-input$gene_search;z<-full();hit<-which(z$gene_id==input$gene_search);if(length(hit)){select_gene(z$gene_id[hit[1]])}})
 observeEvent(input$reset_view,{rv$search<-'';rv$selected<-character();updateTextInput(session,'gene_search',value='');updateSelectInput(session,'class_filter',selected='all');updateCheckboxInput(session,'selected_only',value=FALSE)})
 output$counts_cards<-renderUI({req(rv$obj);z<-full();keys<-names(class_labels);counts<-table(factor(z$display_state,levels=keys));div(class='metricgrid',div(class='metric',span('输入基因'),strong(nrow(z))),div(class='metric',span('保留基因'),strong(sum(z$status!='filtered'))),lapply(keys,function(k)div(class='metric',span(lbl(k)),strong(unname(counts[k])))),p(class='quiet',sprintf('当前干预 %s；上方为全体输入基因计数。未确定 = 原始扰动未获支持 + 分类证据不足；其比例分母为保留基因。',input$intervention)))})
 output$filter_note<-renderUI({z<-filtered();p(sprintf('当前视图 %s 行 / 本干预全量 %s 行；框选 %s 个基因；地图只绘有效估计。',nrow(z),nrow(full()),length(rv$selected)))})
 output$mode_note<-renderUI(if(input$mode=='BH')p('探索视图仅显示已有 BH 类别统计字段；地图仍明确显示主 BY 状态，不拼造 BH 类别。')else p('主 BY：显示全部有效估计，颜色表示正式类别或未确定原因。'))
 pd<-reactive({req(rv$obj,input$intervention%in%unique(rv$obj$results$intervention));rs_plot_data(rv$obj,rv$counts,rv$samples,rv$obj$analysis_id,input$intervention,gene_ids=rv$gene)})
 output$map_frame<-renderUI({sty<-style();plotly::plotlyOutput('response_map',height=paste0(round(730*sty$height/sty$width),'px'))})
 output$response_map<-plotly::renderPlotly({z<-filtered();z<-z[is.finite(z$d)&is.finite(z$t),];validate(need(nrow(z)>0L,'当前筛选没有可绘制的有效估计；重置筛选可返回全量地图。'));m<-rv$obj$config$epsilon_R;xr<-range(z$d);yr<-range(z$t);sty<-style();
  shared<-pd()$map;z<-shared[match(z$gene_id,shared$gene_id),,drop=FALSE];z$key<-paste(rv$obj$analysis_id,z$intervention,z$gene_id,sep='␟');z$display_state<-z$class_key;z$state_label<-unname(rs_class_labels(sty$language)[z$class_key])
  p<-plotly::plot_ly(z,x=~d,y=~t,key=~key,source='response',type='scattergl',mode='markers',color=~factor(state_label,levels=unname(rs_class_labels(sty$language))),colors=unname(sty$palette),marker=list(size=4*sty$point_size,opacity=.55),text=~paste(htmltools::htmlEscape(gene_id),'<br>d=',fmt(d),'; t=',fmt(t),'<br>',lbl(display_state)),hoverinfo='text')
  label_ids<-intersect(sty$label_ids,z$gene_id);if(!length(label_ids)&&sty$label_n>0)label_ids<-head(z$gene_id[order(-abs(z$t),z$gene_id)],sty$label_n)
  if(length(label_ids)){zz<-z[z$gene_id%in%label_ids,];p<-plotly::add_trace(p,data=zz,x=~d,y=~t,key=~key,text=~gsub('\n','<br>',htmltools::htmlEscape(rs_short_label(gene_id)),fixed=TRUE),type='scatter',mode='text',textposition='top center',inherit=FALSE,showlegend=FALSE,hoverinfo='skip')}
  p<-plotly::layout(p,xaxis=list(title='d = D − C (log2)'),yaxis=list(title='t = T − D (log2)'),font=list(family=sty$font_family,size=sty$font_size),dragmode='select',legend=list(orientation='h',y=-.2),margin=list(b=110),shapes=list(list(type='path',path=sprintf('M %g,%g L %g,%g L %g,%g L %g,%g Z',xr[1],-xr[1]-m,xr[2],-xr[2]-m,xr[2],-xr[2]+m,xr[1],-xr[1]+m),fillcolor='rgba(34,116,165,0.08)',line=list(width=0),layer='below'),list(type='line',x0=xr[1],x1=xr[2],y0=0,y1=0,line=list(dash='dash',color='#666666')),list(type='line',x0=xr[1],x1=xr[2],y0=-xr[1],y1=-xr[2],line=list(color='#667788'))))
  p<-plotly::event_register(p,'plotly_click');p<-plotly::event_register(p,'plotly_selected');plotly::config(p,displaylogo=FALSE,modeBarButtonsToRemove=list('sendDataToCloud'))
 })
 observeEvent(plotly::event_data('plotly_click',source='response'),{e<-plotly::event_data('plotly_click',source='response');if(!is.null(e$key)){z<-full();at<-match(e$key[1],z$key);if(!is.na(at)){select_gene(z$gene_id[at]);updateTabsetPanel(session,'worktab',selected='基因详情')}}})
 observeEvent(plotly::event_data('plotly_selected',source='response'),{e<-plotly::event_data('plotly_selected',source='response');if(!is.null(e$key))rv$selected<-intersect(e$key,full()$key)})
 output$results_table<-DT::renderDT({z<-filtered();if(input$mode=='BH'){c<-rv$obj$claims;c<-c[c$intervention==input$intervention & c$gene_id%in%z$gene_id,c('gene_id','intervention','class_name','direction','p_raw','adjusted_p_BH','family_id')];names(c)[6]<-'探索_BH_adjusted_p';return(DT::datatable(c,escape=TRUE,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE)))}
  d<-z[,c('gene_id','intervention','d','t','r','display_state')];d$display_state<-lbl(d$display_state);if(isTRUE(input$advanced_columns)){cols<-grep('(_se$|_lower$|_upper$|^status$|^reason$)',names(z),value=TRUE);d<-cbind(d,z[,cols,drop=FALSE])}
  callback<-DT::JS(sprintf("table.on('click','tbody tr',function(){var d=table.row(this).data();if(d){var el=document.createElement('div');el.innerHTML=d[0];Shiny.setInputValue('table_gene',window.rsGeneIntent({gene_id:el.textContent,analysis_id:%s,intervention:%s}),{priority:'event'});}});",jsonlite::toJSON(rv$obj$analysis_id,auto_unbox=TRUE),jsonlite::toJSON(input$intervention,auto_unbox=TRUE)))
  DT::datatable(d,escape=TRUE,rownames=FALSE,selection='none',callback=callback,options=list(pageLength=10,scrollX=TRUE,language=list(search='表内检索:',lengthMenu='每页 _MENU_ 行',info='第 _START_–_END_ 行，共 _TOTAL_ 行',paginate=list(previous='上一页',`next`='下一页'))))},server=TRUE)
 observeEvent(input$table_gene,{e<-input$table_gene;if(identical(e$analysis_id,rv$obj$analysis_id)&&identical(e$intervention,input$intervention)&&e$gene_id%in%full()$gene_id){select_gene(e$gene_id,client_seq=e$client_seq);updateTabsetPanel(session,'worktab',selected='基因详情')}})
 observeEvent(input$gene_pick_event,{e<-input$gene_pick_event;if(identical(e$analysis_id,rv$obj$analysis_id)&&identical(e$intervention,input$intervention)&&identical(as.integer(e$epoch),rv$selection_epoch))select_gene(e$gene_id,client_seq=e$client_seq)})
 observeEvent(input$intervention,{if(!is.null(rv$gene)&&!is.null(rv$obj)&&input$intervention%in%rv$obj$results$intervention)select_gene(rv$gene,input$intervention)},ignoreInit=TRUE)
 gene_row<-reactive({req(rv$gene);full()[full()$gene_id==rv$gene,,drop=FALSE]})
 output$gene_identity<-renderUI({z<-gene_row();p(id='gene_identity_text',paste(rv$obj$analysis_id,input$intervention,rv$gene,sep=' · '))})
 output$gene_explanation<-renderUI({z<-gene_row();req(nrow(z)>0L);k<-z$display_state[1];sentence<-switch(k,partial_reversal='干预后仍存在同向残余偏离，当前预设下支持部分回调。',within_reference='当前预设下，等效范围与最小改善条件共同支持参考范围内恢复。',overshoot='当前预设下支持越过参考容许范围，不能仅称为完全恢复。',further_deviation='当前预设下支持沿原扰动方向进一步偏离。',no_meaningful_change='当前预设下，有等效证据支持干预效应位于无实质改变范围。',unconfirmed_perturbation='尚未获得原始扰动证据，因此不作扰动回调声明；这不证明原始扰动不存在。',filtered='该基因被低表达规则过滤，不能解释为无变化。','当前必要条件的联合证据不足，不能获得正式分类；这不证明干预无效。');div(h3(lbl(k)),p(sentence))})
 output$sample_plot<-plotly::renderPlotly({d<-pd();d$sample_scale<-input$expression_scale;plotly::ggplotly(rs_product_plot(d,'samples',style()))})
 output$effect_plot<-plotly::renderPlotly({plotly::ggplotly(rs_product_plot(pd(),'effects',style()))})
 output$effect_values<-DT::renderDT({z<-gene_row();req(nrow(z)>0L);cols<-intersect(c('gene_id','intervention','d','d_se','d_lower','d_upper','t','t_se','t_lower','t_upper','r','r_se','r_lower','r_upper','df'),names(z));DT::datatable(z[,cols,drop=FALSE],rownames=FALSE,options=list(dom='t',scrollX=TRUE))})
 output$gene_claims<-DT::renderDT({req(rv$obj,rv$gene);z<-rv$obj$claims;z<-z[z$gene_id==rv$gene & z$intervention==input$intervention,c('class_name','direction','p_raw','adjusted_p_BY','adjusted_p_BH','family_id','rejected')];names(z)[names(z)=='adjusted_p_BH']<-'exploratory_BH_adjusted_p';names(z)[names(z)=='rejected']<-'primary_BY_rejected';DT::datatable(z,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE,dom='t'))})
 output$condition_table<-DT::renderDT({req(rv$obj,rv$gene);DT::datatable(rs_condition_evidence(rv$obj,rv$gene,input$intervention),rownames=FALSE,options=list(dom='t',scrollX=TRUE))})
 output$condition_details<-renderUI({z<-gene_row();req(nrow(z)>0L);c<-rv$obj$config;tags$div(p(sprintf('当前阈值（log2）：δD=%s；εR=%s；δM=%s；ε0=%s。',c$delta_D,c$epsilon_R,c$delta_M,c$epsilon_0)),p('类别内部取各必要条件未校正单侧 P 的最大值，再在完整族内 BY 校正。上表列出两固定方向；未拒绝的声明表示尚不能共同支持其全部条件。'),tags$ul(tags$li('共同必要条件：s·d > δD'),tags$li('参考内恢复：|r| < εR 且 −s·t > δM'),tags$li('部分回调：s·r > εR 且 −s·t > δM'),tags$li('越过参考：s·r < −εR 且 −s·t > δM'),tags$li('进一步偏离：s·t > δM；无实质改变：|t| < ε0')))} )
 output$comparison_note<-renderUI({req(rv$obj);if(length(unique(rv$obj$results$intervention))==1)p('本数据只有一个干预，没有 T_A−T_B 直接比较。可从首页打开多干预模拟示例。')else p('以相同基因集合查看所有干预；默认显示选定基因，颜色为主 BY 状态。')})
 output$matrix_plot<-plotly::renderPlotly({req(rv$obj);req(length(unique(rv$obj$results$intervention))>1);plotly::ggplotly(rs_product_plot(pd(),'matrix',style()))})
 output$direct_table<-DT::renderDT({req(rv$obj);d<-rv$obj$direct_comparisons;d<-d[d$gene_id==rv$gene,];DT::datatable(d,rownames=FALSE,options=list(dom='t',scrollX=TRUE))})
 bundle<-function(){req(rv$obj,rv$counts);sty<-style();z<-filtered();key<-digest::digest(list(rv$obj$analysis_id,sty,input$intervention,rv$gene,z[,c('gene_id','intervention')],input$mode));path<-file.path(tmp,key);if(!file.exists(file.path(path,'EXPORT_README.txt'))){rs_export_bundle(rv$obj,rv$counts,rv$samples,rv$obj$analysis_id,path,sty,input$intervention,rv$gene,z[,c('gene_id','intervention')],paste('gene_id包含',rv$search,';主BY状态',input$class_filter,';框选',input$selected_only),root)};path}
 output$export_note<-renderUI({req(rv$obj);p(sprintf('分析版本 %s；全量 %s 行（所有干预），当前筛选 %s 行。候选表仅主 BY 的参考内恢复/部分回调，允许为空。',rv$obj$analysis_id,nrow(rv$obj$results),nrow(filtered())))})
 for(nm in c('template_counts','template_samples'))local({id<-nm;output[[id]]<-downloadHandler(filename=function()paste0(id,'.tsv'),content=function(file){if(id=='template_counts')write.table(data.frame(gene_id=c('gene_1','gene_2'),C1=c(50,80),C2=c(55,85),C3=c(60,88),D1=c(100,50),D2=c(110,45),D3=c(120,48),T1=c(70,70),T2=c(75,73),T3=c(80,75)),file,sep='\t',row.names=FALSE,quote=FALSE)else write.table(data.frame(sample_id=paste0(rep(c('C','D','T'),each=3),1:3),group=rep(c('Control','Perturbation','Treatment'),each=3),role=rep(c('C','D','T'),each=3)),file,sep='\t',row.names=FALSE,quote=FALSE)})})
 # Product filenames are resolved by semantic role, never by a browser page of rows.
 files<-c(dl_full_tsv='full_results_BY.tsv',dl_full_csv='full_results_BY.csv',dl_filtered='filtered_results_BY.tsv',dl_candidates='candidates_BY.tsv',dl_svg='map.svg',dl_pdf='map.pdf',dl_html='report.html')
 for(nm in names(files))local({id<-nm;fn<-files[[id]];output[[id]]<-downloadHandler(filename=function()paste0(rv$obj$analysis_id,'_BY_',fn),content=function(file){req(rv$obj,rv$obj$analysis_id)
  if(id%in%c('dl_svg','dl_pdf')){rs_save_plot(rs_product_plot(pd(),'map',style()),file,style(),if(id=='dl_svg')'svg' else 'pdf');return()}
  if(id!='dl_html'){z<-rv$obj$results;if(id=='dl_filtered')z<-filtered()[,names(rv$obj$results),drop=FALSE];if(id=='dl_candidates')z<-z[!is.na(z$classification)&z$classification%in%c('within_reference','partial_reversal'),,drop=FALSE];z<-z[,!names(z)%in%c('d_padj','t_padj','r_padj'),drop=FALSE];z$analysis_id<-rep(rv$obj$analysis_id,nrow(z));z$method<-rep('BY',nrow(z));z$export_scope<-rep(if(id=='dl_filtered')paste('当前筛选:',rv$search,input$class_filter)else if(id=='dl_candidates')'主BY参考范围内恢复或部分回调候选'else '所有输入基因和全部干预',nrow(z));write.table(z,file,sep=if(id=='dl_full_csv')','else '\t',quote=TRUE,row.names=FALSE,na='NA',fileEncoding='UTF-8');return()}
  rs_export_report(rv$obj,rv$counts,rv$samples,rv$obj$analysis_id,file,style(),input$intervention,rv$gene,filtered()[,c('gene_id','intervention')],paste('当前筛选',rv$search,input$class_filter),root)})})
 output$dl_bh<-downloadHandler(filename=function()paste0(rv$obj$analysis_id,'_exploratory_BH_claims.tsv'),content=function(file){req(rv$obj,rv$obj$analysis_id);z<-rv$obj$claims[,c('gene_id','intervention','class_name','direction','p_raw','adjusted_p_BH','family_id')];z$analysis_id<-rep(rv$obj$analysis_id,nrow(z));z$method<-rep('BH exploratory; no derived classification',nrow(z));write.table(z,file,sep='\t',quote=TRUE,row.names=FALSE,fileEncoding='UTF-8')})
 output$dl_bundle<-downloadHandler(filename=function()paste0(rv$obj$analysis_id,'_BY_成果.zip'),content=function(file){b<-bundle();zip::zipr(file,list.files(b,recursive=TRUE,full.names=FALSE),root=b,mode='mirror')})
 output$dl_project<-downloadHandler(filename=function()paste0('RevertScope_',rv$obj$analysis_id,'.zip'),content=function(file){req(rv$obj,rv$project);rs_project_save_view(rv$project,view(),style(),rv$obj$analysis_id);rs_project_export(rv$project,file)})
 output$diagnostics<-renderPrint({list(software=rs_release()$software_version,method=rs_release()$method_version,schema=rs_release()$schema_version,build_id=rs_release()$build_id,project=if(is.null(rv$project))NULL else rv$project$path,analysis=if(is.null(rv$obj))NULL else rv$obj$analysis_id,task=rv$last_task,error=rv$error)})
 for(nm in c('notice','import_error','input_summary','role_summary','task_status','project_title','analysis_identity','counts_cards','filter_note','mode_note','gene_identity','gene_explanation','condition_details','comparison_note','export_note','analysis_history'))outputOptions(output,nm,suspendWhenHidden=FALSE)
}
shinyApp(ui,server)
