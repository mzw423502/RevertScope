# Base-R implementation of the figure-style role ladder and outward ticks.
apply_figure_style <- function() {
  graphics::par(family="sans",las=1,tcl=-0.25,bty="l",cex.axis=0.82,
                cex.lab=1,cex.main=1,font.main=1,mgp=c(2.5,0.65,0))
}

.rs_export_schema <- function(object) {
  if(!identical(object$schema_version,"2.0.0"))
    stop("[SCHEMA_VERSION] Export and plotting require schema 2.0.0. Refit original counts with the four-threshold configuration; do not silently read round-1 results.",call.=FALSE)
  needed <- c("results","claims","direct_comparisons","perturbation_diagnostics","families","sample_mapping","config")
  if(!all(needed %in% names(object))) stop("[RESULT_SCHEMA] Incomplete schema 2.0.0 object: regenerate the canonical fit before exporting.",call.=FALSE)
  invisible(object)
}

plot_response <- function(object, intervention=NULL) {
  .rs_export_schema(object)
  if(is.null(intervention)) intervention <- unique(object$results$intervention)[1]
  z <- object$results[object$results$intervention==intervention &
                       is.finite(object$results$d) & is.finite(object$results$t),,drop=FALSE]
  if(!nrow(z)) stop("No estimable retained genes to plot.",call.=FALSE)
  keys <- c("within_reference","partial_reversal","overshoot","further_deviation",
            "no_meaningful_change","insufficient_evidence","unconfirmed_perturbation")
  labels <- c("Within reference range","Partial reversal","Beyond reference range",
              "Further deviation","No meaningful change","Insufficient evidence",
              "Perturbation unconfirmed")
  palette <- c("#0072B2","#009E73","#CC79A7","#D55E00","#E69F00","#777777","#BBBBBB")
  k <- as.character(z$classification)
  k[is.na(k) | !nzchar(k)] <- as.character(z$status[is.na(k) | !nzchar(k)])
  extra <- setdiff(unique(k),keys)
  keys <- c(keys,extra); labels <- c(labels,extra); palette <- c(palette,rep("#888888",length(extra)))
  old <- graphics::par(no.readonly=TRUE); on.exit(graphics::par(old))
  graphics::layout(matrix(c(1,2),nrow=1),widths=c(3.3,1.7))
  on.exit(graphics::layout(1),add=TRUE)
  apply_figure_style(); graphics::par(mar=c(4.5,4.5,3.2,0.8))
  xr <- range(z$d); yr <- range(z$t)
  xr <- xr+c(-1,1)*max(diff(xr)*0.06,0.15)
  yr <- yr+c(-1,1)*max(diff(yr)*0.06,0.15)
  graphics::plot(z$d,z$t,type="n",xlim=xr,ylim=yr,
                 xlab="Perturbation d = D - C (log2)",ylab="Intervention t = T - D (log2)",
                 main=paste("Response map:",intervention))
  m <- object$config$epsilon_R
  graphics::polygon(c(xr,rev(xr)),c(-xr-m,rev(-xr+m)),col="#E8EFF5",border=NA)
  graphics::abline(h=0,lty=2,col="#555555",lwd=1)
  graphics::abline(a=0,b=-1,lty=1,col="#333333",lwd=1)
  for(bound in c(-m,m)) graphics::abline(a=bound,b=-1,lty=3,col="#668099")
  for(i in seq_along(keys)) {
    sel <- k==keys[i]
    graphics::points(z$d[sel],z$t[sel],pch=if(keys[i]=="unconfirmed_perturbation") 1 else 16,
                     cex=0.42,col=grDevices::adjustcolor(palette[i],alpha.f=0.65))
  }
  graphics::par(mar=c(4.5,0,3.2,0.5)); graphics::plot.new()
  present <- keys %in% unique(k)
  graphics::legend("topleft",legend=labels[present],col=palette[present],
                   pch=ifelse(keys[present]=="unconfirmed_perturbation",1,16),
                   bty="n",cex=0.78,pt.cex=0.7,x.intersp=0.6,y.intersp=1.25)
  graphics::legend("bottomleft",legend=c("t = 0","r = 0 (t = -d)",
    paste0("Reference band: |r| < ",m)),lty=c(2,1,3),
    col=c("#555555","#333333","#668099"),bty="n",cex=0.78)
  graphics::mtext(paste0(nrow(z)," retained genes; BY class claims"),side=1,line=2.1,cex=0.75)
  invisible(z)
}

export_response <- function(object,out) {
  .rs_export_schema(object)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  # One canonical object: no subset-specific correction or refitting in export.
  saveRDS(object,file.path(out,"result.rds"),version=3)
  write_tsv <- function(x,name) utils::write.table(x,file.path(out,paste0(name,".tsv")),sep="\t",quote=FALSE,row.names=FALSE,na="NA",fileEncoding="UTF-8")
  for(nm in c("results","claims","perturbation_diagnostics","direct_comparisons","families","sample_mapping")) write_tsv(object[[nm]],nm)
  tab <- as.data.frame(table(object$results$intervention,object$results$status,object$results$classification,useNA="ifany"))
  names(tab) <- c("intervention","status","classification","n");tab <- tab[tab$n>0,,drop=FALSE]
  write_tsv(tab,"summary")
  candidates <- object$results[object$results$status=="classified" & object$results$classification %in% c("within_reference","partial_reversal"),,drop=FALSE]
  write_tsv(candidates,"candidates")
  arms <- unique(object$results$intervention)
  maps <- data.frame(intervention=arms,stem=if(length(arms)==1)"response_map" else paste0("response_map_",seq_along(arms)))
  for(i in seq_along(arms)) {
    arm <- arms[i];stem <- maps$stem[i]
    grDevices::png(file.path(out,paste0(stem,".png")),width=3000,height=1800,res=300)
    tryCatch(plot_response(object,arm),finally=grDevices::dev.off())
    grDevices::svg(file.path(out,paste0(stem,".svg")),width=10,height=6)
    tryCatch(plot_response(object,arm),finally=grDevices::dev.off())
    grDevices::pdf(file.path(out,paste0(stem,".pdf")),width=10,height=6)
    tryCatch(plot_response(object,arm),finally=grDevices::dev.off())
  }
  manifest <- list(schema_version=object$schema_version,method_version=object$method_version,
    canonical_result="result.rds",canonical_sha256=digest::digest(file=file.path(out,"result.rds"),algo="sha256"),
    provenance=object$provenance,config=object$config,diagnostics=object$diagnostics,families=object$families,
    sample_mapping=object$sample_mapping,map_files=maps,
    tables=stats::setNames(lapply(c("results","claims","perturbation_diagnostics","direct_comparisons","families","sample_mapping"),function(nm)list(rows=nrow(object[[nm]]),columns=names(object[[nm]]))),c("results","claims","perturbation_diagnostics","direct_comparisons","families","sample_mapping")),
    interpretation=list(intervals="95% marginal moderated-t; not simultaneous",primary="BY class claims across all retained genes x planned T x 5 classes x 2 fixed directions",sensitivity="BH on the same full family; exploratory",separate_families="Classification, perturbation diagnostics and direct comparisons have separate guarantees, not one combined guarantee",adjusted_p="Adjusted p-values are not individual classification-error probabilities"))
  jsonlite::write_json(manifest,file.path(out,"run_manifest.json"),pretty=TRUE,auto_unbox=TRUE,na="null")
  writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
  lines <- c("# RevertScope analysis: schema 2.0.0", "",
    "Primary classifications are directional intersection-union claims with BY correction over the prespecified complete class family. Empirical validation scope is reported in docs/round2.md; this output is not a publication or registration claim.",
    "",paste0("Method version: ",object$method_version,"; result rows: ",nrow(object$results),"; retained class hypotheses: ",nrow(object$claims)),"", "## Classification counts", "",capture.output(print(tab,row.names=FALSE)),
    "", "## Interpretation", "",
    "Within-reference recovery requires equivalent residual effect within the prespecified reference band and a meaningful opposite intervention effect, together with original perturbation support in the class IUT. A nonsignificant T-C comparison does not prove recovery.",
    "No meaningful change requires equivalence of T-D within epsilon_0; nonsignificance does not establish this claim.",
    "Intervals are ordinary 95% marginal moderated-t intervals. They are distinct from BY-adjusted class claims and do not jointly cover all genes or contrasts.",
    "Adjusted p-values are not the probability that an individual gene classification is wrong. BY assumes valid input p-values; voom and empirical-Bayes inference is model-dependent.",
    "Classification, original-perturbation diagnostics, and direct T_A-T_B comparisons are separate BY families. No combined overall guarantee across the three families is claimed. BH results use the same full class family and are exploratory sensitivity output.",
    "Unconfirmed original perturbation, filtered genes, insufficient intervention evidence and conflicts retain separate status/reason fields. An unconfirmed perturbation does not establish its absence.",
    "The map includes all estimable retained genes. Its reference band describes point-estimate geometry and is not an equivalence decision. Filtered genes are excluded from the map.",
    "", "## Files", "",
    "result.rds is the canonical object. results.tsv retains all input genes and reasons; claims.tsv retains every prespecified gene/T/class/direction hypothesis, including unsupported claims. Interface filtering must not recalculate the family.",
    "perturbation_diagnostics.tsv, direct_comparisons.tsv and families.tsv preserve diagnostic and direct-comparison families. Empty direct comparisons still have headers. sample_mapping.tsv preserves fitted sample identities and metadata.",
    "candidates.tsv contains only main-BY within-reference or partial-reversal claims. SVG and PDF are editable vector maps; PNG is a preview. The manifest records schema, method, thresholds, families, sample mapping, input provenance and the canonical RDS SHA-256.")
  writeLines(lines,file.path(out,"report.md"),useBytes=TRUE)
  invisible(object)
}
