# Fits supplied example counts using the installed candidate, then creates projects.
.Library.site<-character();.libPaths(c(normalizePath('.library'),.Library),include.site=FALSE)
library(revertscope)
source('R/000_release.R',encoding='UTF-8');source('R/projects.R',encoding='UTF-8');source('R/product_export.R',encoding='UTF-8')
for(which in c('multi','public')) {
 base<-if(which=='multi')'examples/multi' else 'data/public/GSE208041'
 if(!file.exists(file.path(base,'counts.tsv')))next
 target<-file.path('examples/projects',which)
 if(file.exists(file.path(target,'project.json')))next
 cfg<-yaml::read_yaml(if(which=='multi')'examples/multi/config.yml' else 'config/public_GSE208041.yml')
 x<-read_inputs(file.path(base,'counts.tsv'),file.path(base,'samples.tsv'),cfg);set.seed(cfg$seed)
 o<-fit_response(x$counts,x$samples,x$config);o$provenance<-list(source=if(which=='multi')'Fixed simulated multi-intervention example; not a public or customer dataset' else 'GSE208041 original count reanalysis; same-time cotreatment',seed=cfg$seed)
 p<-rs_project_create(if(which=='multi')'固定多干预模拟' else 'GSE208041 真实公开案例',base='examples/projects')
 p<-rs_project_add_result(p,o,file.path(base,'counts.tsv'),file.path(base,'samples.tsv'),cfg)
 stopifnot(!dir.exists(target));stopifnot(file.rename(p$path,target))
 cat('Prepared actual count fit:',which,'\n')
}
