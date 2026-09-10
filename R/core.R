# RevertScope development computational kernel. See docs/METHODS.md.

.rs_fail <- function(code, message) {
  stop(structure(list(message = paste0("[", code, "] ", message), call = NULL,
                      code = code), class = c("revertscope_error", "error", "condition")))
}

.rs_config <- function(config) {
  if (is.character(config) && length(config) == 1L) config <- yaml::read_yaml(config)
  if (!is.list(config)) .rs_fail("CONFIG", "Provide a YAML configuration or named list.")
  if (any(c("reference_margin", "meaningful_change") %in% names(config)))
    .rs_fail("CONFIG_SCHEMA", "Round-1 thresholds are unsupported; explicitly choose delta_D, epsilon_R, delta_M, epsilon_0 and refit.")
  defaults <- list(delta_D = 0.75, epsilon_R = 0.5, delta_M = 0.5, epsilon_0 = 0.25, alpha = 0.05,
                   min_replicates = 3L, min_count = 10, min_total_count = 15,
                   max_cells = 5e6)
  config <- utils::modifyList(defaults, config)
  for (nm in c("delta_D", "epsilon_R", "delta_M", "epsilon_0", "alpha", "min_replicates",
               "min_count", "min_total_count", "max_cells")) {
    if (!is.numeric(config[[nm]]) || length(config[[nm]]) != 1L ||
        !is.finite(config[[nm]]) || config[[nm]] <= 0) {
      .rs_fail("CONFIG", paste("Set", nm, "to one finite positive number."))
    }
  }
  if (!(config$delta_D > config$epsilon_R && config$delta_M > config$epsilon_0))
    .rs_fail("THRESHOLD_ORDER", "Require delta_D > epsilon_R > 0 and delta_M > epsilon_0 > 0 on the log2 scale.")
  if (config$alpha >= 1) .rs_fail("CONFIG", "Set alpha strictly between 0 and 1.")
  if (config$min_replicates < 2 || config$min_replicates %% 1 != 0)
    .rs_fail("CONFIG", "min_replicates must be an integer >= 2; technical replicates do not count.")
  if (!identical(config$data_scale, "raw_counts") || !isTRUE(config$source_confirmed))
    .rs_fail("SOURCE_CONFIRMATION", "Confirm the source is raw gene counts: data_scale: raw_counts and source_confirmed: true. Do not round TPM, FPKM or log-expression.")
  if (!isTRUE(config$independent_replicates) || !identical(config$design_type, "independent"))
    .rs_fail("UNSUPPORTED_DESIGN", "Only independent biological replicates are supported. Set independent_replicates: true and design_type: independent only after checking the experiment; paired, longitudinal, single-cell and mixed designs are unsupported.")
  config
}

.rs_read_table <- function(path) {
  if (!file.exists(path)) .rs_fail("FILE_MISSING", paste("File does not exist:", path))
  ext <- tolower(tools::file_ext(path))
  ans <- tryCatch(switch(ext,
    csv = utils::read.table(path, header = TRUE, sep = ",", quote = "\"", comment.char = "",
      check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character", na.strings = ""),
    tsv = utils::read.table(path, header = TRUE, sep = "\t", quote = "\"", comment.char = "",
      check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character", na.strings = ""),
    xlsx = as.data.frame(readxl::read_xlsx(path, col_types = "text", .name_repair = "minimal"),
                         stringsAsFactors = FALSE),
    .rs_fail("FORMAT", "Use .csv, .tsv, or .xlsx. The first count column must be gene_id.")),
    error = function(e) {
      if (inherits(e, "revertscope_error")) stop(e)
      .rs_fail("TABLE_PARSE", paste("Check delimiters, row lengths and encoding in", path, ":", conditionMessage(e)))
    })
  if (!nrow(ans) || !ncol(ans)) .rs_fail("EMPTY_TABLE", paste("Supply a nonempty table:", path))
  if (anyNA(names(ans)) || any(!nzchar(trimws(names(ans)))) || anyDuplicated(names(ans)))
    .rs_fail("DUPLICATE_COLUMN", paste("Fix blank or duplicate column IDs in", path))
  ans
}

#' Read raw counts and sample metadata without rounding or imputation
#' @param counts_path CSV/TSV/XLSX with gene_id then sample columns.
#' @param samples_path CSV/TSV/XLSX with sample_id, group, role.
#' @param config Named list or YAML path.
#' @return A validated list with counts, samples, config and design information.
#' @export
read_inputs <- function(counts_path, samples_path, config) {
  config <- .rs_config(config)
  raw <- .rs_read_table(counts_path)
  if (names(raw)[1L] != "gene_id") .rs_fail("GENE_ID_COLUMN", "Name the first count column gene_id; keep all remaining columns as sample counts.")
  if (nrow(raw) * (ncol(raw) - 1) > config$max_cells)
    .rs_fail("SIZE_LIMIT", "Count matrix exceeds configured max_cells. Review available memory before explicitly increasing this limit.")
  ids <- raw[[1L]]
  if (anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids))
    .rs_fail("GENE_ID", "Fix missing or duplicate gene_id values using documented gene identities; no automatic merging is performed.")
  counts <- suppressWarnings(matrix(as.numeric(as.matrix(raw[-1L])), nrow = nrow(raw),
                                    dimnames = list(ids, names(raw)[-1L])))
  validate_inputs(counts, .rs_read_table(samples_path), config)
}

#' Validate independent bulk RNA-seq counts, roles and estimable fixed-effect design
#' @param counts Numeric gene-by-sample raw count matrix with dimnames.
#' @param samples Sample metadata data.frame.
#' @param config Named configuration list or YAML path.
#' @return Validated list; failures are actionable revertscope_error conditions.
#' @export
validate_inputs <- function(counts, samples, config) {
  config <- .rs_config(config)
  if (!is.matrix(counts) || !is.numeric(counts) || nrow(counts) < 2L || ncol(counts) < 3L)
    .rs_fail("COUNT_MATRIX", "Provide a numeric gene-by-sample matrix with at least two genes and C, D, T samples.")
  if (length(counts) > config$max_cells)
    .rs_fail("SIZE_LIMIT", "Count matrix exceeds configured max_cells. Review available memory before explicitly increasing this limit.")
  for (ids in list(rownames(counts), colnames(counts))) {
    if (is.null(ids) || anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids))
      .rs_fail("DUPLICATE_ID", "Give every gene and sample one unique, nonempty ID. Resolve identities at the source; do not silently merge.")
  }
  if (any(!is.finite(counts))) .rs_fail("COUNT_MISSING", "Fix missing, nonnumeric or infinite counts at the source; counts are never imputed.")
  if (any(counts < 0)) .rs_fail("NEGATIVE_COUNT", "Counts must be nonnegative; export raw gene counts.")
  if (any(abs(counts - round(counts)) > 1e-8))
    .rs_fail("NONINTEGER_COUNT", "Noninteger values were found. Supply raw integer gene counts; do not round normalized expression.")
  if (any(counts > 2^53)) .rs_fail("COUNT_RANGE", "Counts above 2^53 cannot be represented exactly; check the input scale.")
  if (any(colSums(counts) <= 0)) .rs_fail("ZERO_LIBRARY", paste("Zero-count libraries:", paste(colnames(counts)[colSums(counts) <= 0], collapse = ", "), ". Correct or document removal at the source."))
  if (!is.data.frame(samples) || !all(c("sample_id", "group", "role") %in% names(samples)))
    .rs_fail("SAMPLE_COLUMNS", "Sample metadata must contain sample_id, group and role (C, D or T).")
  if (anyDuplicated(names(samples))) .rs_fail("DUPLICATE_COLUMN", "Fix duplicate sample metadata column names.")
  for (nm in intersect(c("sample_id", "group", "role", "batch", "biological_unit"), names(samples))) {
    samples[[nm]] <- as.character(samples[[nm]])
    if (anyNA(samples[[nm]]) || any(!nzchar(trimws(samples[[nm]]))))
      .rs_fail("SAMPLE_MISSING", paste("Fill all", nm, "values from experimental records, or remove an unused optional column."))
  }
  if (anyDuplicated(samples$sample_id)) .rs_fail("SAMPLE_ID", "Each sample_id must occur exactly once in metadata.")
  if (!setequal(colnames(counts), samples$sample_id) || ncol(counts) != nrow(samples)) {
    .rs_fail("SAMPLE_MAPPING", paste0("Match count columns and metadata one-to-one. Missing metadata: ",
      paste(setdiff(colnames(counts), samples$sample_id), collapse = ", "),
      "; missing count columns: ", paste(setdiff(samples$sample_id, colnames(counts)), collapse = ", "), "."))
  }
  unsupported <- intersect(tolower(names(samples)), c("subject", "subject_id", "patient_id", "pair", "pair_id", "time", "timepoint", "cell_id", "donor_id"))
  if (length(unsupported)) .rs_fail("UNSUPPORTED_DESIGN", paste("Unsupported paired/longitudinal/single-cell design metadata:", paste(unsupported, collapse = ", "), ". Establish an independent bulk design before using this prototype; do not merely relabel correlated samples."))
  if ("biological_unit" %in% names(samples) && anyDuplicated(samples$biological_unit))
    .rs_fail("NONINDEPENDENT_UNIT", "Repeated biological_unit values indicate paired/repeated or technical samples; this version cannot treat them as independent replicates.")
  if (!all(samples$role %in% c("C", "D", "T"))) .rs_fail("GROUP_ROLE", "role must be exactly C, D or T for every sample.")
  by_group <- split(samples$role, samples$group)
  if (any(vapply(by_group, function(z) length(unique(z)) != 1L, logical(1))))
    .rs_fail("GROUP_ROLE", "Each group must have exactly one role; resolve mixed C/D/T labels.")
  role_groups <- lapply(c("C", "D", "T"), function(z) unique(samples$group[samples$role == z]))
  names(role_groups) <- c("C", "D", "T")
  if (length(role_groups$C) != 1L || length(role_groups$D) != 1L || !length(role_groups$T))
    .rs_fail("GROUP_ROLE", "Specify exactly one C group, one D group and at least one distinct T group.")
  sizes <- table(samples$group)
  if (any(sizes < config$min_replicates)) .rs_fail("REPLICATES", paste("Insufficient independent biological replicates in:", paste(names(sizes)[sizes < config$min_replicates], collapse = ", "), ". Required per group:", config$min_replicates))
  counts <- counts[, samples$sample_id, drop = FALSE]
  lev <- c(role_groups$C, role_groups$D, role_groups$T)
  group <- factor(samples$group, levels = lev)
  design <- stats::model.matrix(~ 0 + group)
  colnames(design) <- paste0("group_", seq_along(lev))
  group_columns <- stats::setNames(seq_along(lev), lev)
  if ("batch" %in% names(samples) && length(unique(samples$batch)) > 1L) {
    batch <- factor(samples$batch)
    bm <- stats::model.matrix(~ batch)[, -1L, drop = FALSE]
    colnames(bm) <- paste0("batch_", seq_len(ncol(bm)))
    design <- cbind(design, bm)
  }
  rownames(design) <- samples$sample_id
  if (qr(design)$rank < ncol(design)) .rs_fail("DESIGN_NOT_ESTIMABLE", "The group + fixed batch design is not full rank (often group/batch confounding). Add cross-batch group overlap or use a scientifically justified estimable design; do not remove batch solely to obtain results.")
  if (nrow(design) <= ncol(design)) .rs_fail("NO_RESIDUAL_DF", "The design leaves no residual degrees of freedom. Add biological replication or simplify only scientifically unnecessary fixed effects.")
  list(counts = counts, samples = samples, config = config, design = design,
       group_columns = group_columns, C = role_groups$C, D = role_groups$D, T = role_groups$T)
}

.rs_weighted_covariance <- function(X, w) {
  p <- ncol(X)
  if (any(!is.finite(w)) || any(w <= 0)) .rs_fail("WEIGHTED_DESIGN", "Weights must be finite and positive; inspect libraries.")
  q <- qr(sqrt(w) * X)
  if(q$rank < p) .rs_fail("WEIGHTED_DESIGN", "Weighted design is not estimable; inspect batch overlap and libraries.")
  ri <- backsolve(qr.R(q), diag(p)); ans <- matrix(0, p, p)
  ans[q$pivot, q$pivot] <- tcrossprod(ri)
  ans
}

# Fit a unified model; all covariance calculations use weighted QR factors.
fit_response <- function(counts, samples, config) {
  inp <- validate_inputs(counts, samples, config)
  counts <- inp$counts; samples <- inp$samples; config <- inp$config; X <- inp$design
  y <- edgeR::DGEList(counts = counts)
  keep <- edgeR::filterByExpr(y, design = X, min.count = config$min_count, min.total.count = config$min_total_count)
  if (sum(keep) < 2L) .rs_fail("FILTER_EMPTY", "Fewer than two genes pass filtering; check count source and predeclared filtering settings.")
  y <- y[keep, , keep.lib.sizes = FALSE]
  if (any(y$samples$lib.size <= 0)) .rs_fail("FILTER_ZERO_LIBRARY", "A library has no counts after filtering; review library quality and declared filters.")
  y <- edgeR::calcNormFactors(y, method = "TMM")
  v <- limma::voom(y, design = X, plot = FALSE, save.plot = TRUE)
  fit <- limma::eBayes(limma::lmFit(v, design = X), trend = FALSE, robust = FALSE)
  genes <- rownames(counts); kg <- rownames(y); ng <- length(kg); p <- ncol(X)
  # QR of sqrt(W)X avoids squaring its condition number. Undo column pivot.
  covariance_beta <- array(0, c(p, p, ng))
  for (i in seq_len(ng)) {
    covariance_beta[, , i] <- .rs_weighted_covariance(X, v$weights[i, ]) * fit$s2.post[i]
  }
  # Vectorized contraction over genes after computing QR just once per gene.
  contract <- function(a,b) as.vector(crossprod(as.vector(outer(a,b)), matrix(covariance_beta, p*p, ng)))
  make_contrast <- function(a,b) { z <- numeric(p); z[inp$group_columns[a]] <- 1; z[inp$group_columns[b]] <- -1; z }
  ld <- make_contrast(inp$D, inp$C)
  contrast_list <- list(); out <- vector("list", length(inp$T)); pos <- match(kg,genes)
  critical <- stats::qt(0.975, fit$df.total)
  for (j in seq_along(inp$T)) {
    ti <- inp$T[j]; lt <- make_contrast(ti,inp$D); lr <- ld + lt
    L <- cbind(d=ld,t=lt,r=lr); rownames(L) <- colnames(X); contrast_list[[ti]] <- L
    est <- fit$coefficients %*% L
    se <- sqrt(cbind(d=contract(ld,ld),t=contract(lt,lt),r=contract(lr,lr)))
    if(any(!is.finite(se)) || any(se<=0)) .rs_fail("FIT_UNCERTAINTY", "Nonpositive or nonfinite uncertainty; inspect expression variation and libraries.")
    tab <- data.frame(gene_id=genes,intervention=ti,comparison_d=paste(inp$D,"-",inp$C),comparison_t=paste(ti,"-",inp$D),comparison_r=paste(ti,"-",inp$C),stringsAsFactors=FALSE)
    for(nm in c("d","t","r")) {
      for(suffix in c("","_se","_lower","_upper","_p","_padj")) tab[[paste0(nm,suffix)]] <- NA_real_
      tab[[nm]][pos] <- est[,nm];tab[[paste0(nm,"_se")]][pos] <- se[,nm]
      tab[[paste0(nm,"_lower")]][pos] <- est[,nm]-critical*se[,nm]
      tab[[paste0(nm,"_upper")]][pos] <- est[,nm]+critical*se[,nm]
      tab[[paste0(nm,"_p")]][pos] <- 2*stats::pt(-abs(est[,nm]/se[,nm]),fit$df.total)
      # These are descriptive zero-effect BH tests per contrast, not class tests.
      tab[[paste0(nm,"_padj")]][pos] <- stats::p.adjust(tab[[paste0(nm,"_p")]][pos],"BH")
    }
    for(nm in c("cov_dt","cov_dr","cov_tr","df","identity_error","variance_identity_error")) tab[[nm]] <- NA_real_
    tab$cov_dt[pos] <- contract(ld,lt);tab$cov_dr[pos] <- contract(ld,lr);tab$cov_tr[pos] <- contract(lt,lr);tab$df[pos] <- fit$df.total
    tab$identity_error[pos] <- abs(tab$r[pos]-tab$d[pos]-tab$t[pos])
    tab$variance_identity_error[pos] <- abs(tab$r_se[pos]^2-tab$d_se[pos]^2-tab$t_se[pos]^2-2*tab$cov_dt[pos])
    if(any(tab$identity_error[pos]>1e-8) || any(tab$variance_identity_error[pos]>1e-8)) .rs_fail("CONTRAST_IDENTITY", "Effect or covariance identity failed; stop interpretation and report the error.")
    tab$status <- ifelse(keep,"analyzed","filtered");tab$reason <- ifelse(keep,"","low_expression_filterByExpr");tab$classification <- NA_character_;tab$development <- FALSE
    out[[j]] <- tab
  }
  direct <- data.frame(gene_id=character(),intervention_a=character(),intervention_b=character(),comparison=character(),effect=numeric(),se=numeric(),lower=numeric(),upper=numeric(),df=numeric(),raw_p=numeric(),adjusted_p=numeric(),family_id=character())
  if(length(inp$T)>1) {
    pairs <- combn(sort(inp$T),2,simplify=FALSE)
    direct <- do.call(rbind,lapply(pairs,function(pair) {
      lc <- make_contrast(pair[1],pair[2]);e <- as.vector(fit$coefficients%*%lc);s <- sqrt(contract(lc,lc))
      data.frame(gene_id=kg,intervention_a=pair[1],intervention_b=pair[2],comparison=paste(pair[1],"-",pair[2]),effect=e,se=s,lower=e-critical*s,upper=e+critical*s,df=fit$df.total,raw_p=2*stats::pt(-abs(e/s),fit$df.total),family_id="direct_all_genes_all_T_pairs",stringsAsFactors=FALSE)
    }))
    direct$adjusted_p <- stats::p.adjust(direct$raw_p,"BY")
  }
  result <- structure(list(schema_version="2.0.0",method_version="0.2.0",results=do.call(rbind,out),config=config,
    design=X,contrasts=contrast_list,direct_comparisons=direct,sample_mapping=samples,
    fit_inputs=list(expression=v$E,weights=v$weights,coefficients=fit$coefficients,sigma2=fit$sigma^2,
      df_residual=fit$df.residual,df_prior=fit$df.prior,s2_prior=fit$s2.prior,s2_post=fit$s2.post,df_total=fit$df.total,posterior_variance=fit$s2.post),
    filter=data.frame(gene_id=genes,retained=keep),
    diagnostics=list(n_input_genes=nrow(counts),n_retained_genes=sum(keep),n_samples=ncol(counts),design_rank=qr(X)$rank,residual_df=nrow(X)-ncol(X),
      max_identity_error=max(vapply(out,function(z)max(z$identity_error,na.rm=TRUE),numeric(1))),
      normalization="TMM",transformation="limma-voom",interval_method="95% marginal moderated-t; not simultaneous",
      interval_marginal_confidence=0.95,genome_wide_classification_error_control="BY over all retained genes x T x 5 classes x 2 fixed directions; conditional on valid moderated-t p-values",
      annotation_only_metadata=setdiff(names(samples),c("sample_id","group","role","batch","biological_unit")),sample_order=samples$sample_id,
      library_sizes=as.list(stats::setNames(colSums(counts),colnames(counts))),normalization_factors=as.list(stats::setNames(y$samples$norm.factors,samples$sample_id)),
      versions=c(list(revertscope="0.2.0",R=as.character(getRversion())),as.list(vapply(c("limma","edgeR","readxl","yaml","jsonlite"),function(p)as.character(utils::packageVersion(p)),character(1))))
    )),class="revertscope_result")
  classify_response(result)
}

# Fixed-direction intersection-union tests, then a single global BY family.
classify_response <- function(result, config=result$config) {
  if(!identical(result$schema_version,"2.0.0")) .rs_fail("SCHEMA_VERSION", "Only schema 2.0.0 is supported. Refit original counts with the new four-threshold configuration; round-1 intervals cannot be silently migrated.")
  config <- .rs_config(config)
  if(!isTRUE(all.equal(config,.rs_config(result$config)))) .rs_fail("REFIT_REQUIRED", "Configuration differs from fitted provenance. Refit counts with the explicitly chosen configuration.")
  z <- result$results
  if(!all(z$status %in% c("filtered","analyzed","unconfirmed_perturbation","classified","conflict"))) .rs_fail("RESULT_STATE", "Only successfully fitted or filtered rows can be classified; fix nonestimable designs first.")
  ok <- which(z$status!="filtered"); x <- z[ok,,drop=FALSE];N <- nrow(x)
  upper <- function(e,se,b) stats::pt((b-e)/se,x$df)
  lower <- function(e,se,b) stats::pt((e-b)/se,x$df)
  eqr <- pmax(upper(x$r,x$r_se,-config$epsilon_R),lower(x$r,x$r_se,config$epsilon_R))
  eqt <- pmax(upper(x$t,x$t_se,-config$epsilon_0),lower(x$t,x$t_se,config$epsilon_0))
  classes <- c("within_reference","partial_reversal","overshoot","further_deviation","no_meaningful_change")
  chunks <- vector("list",10L);k <- 0L
  for(s in c(1,-1)) {
    pd <- upper(s*x$d,x$d_se,config$delta_D);pm <- upper(-s*x$t,x$t_se,config$delta_M)
    pp <- list(pmax(pd,pm,eqr),pmax(pd,pm,upper(s*x$r,x$r_se,config$epsilon_R)),pmax(pd,pm,lower(s*x$r,x$r_se,-config$epsilon_R)),pmax(pd,upper(s*x$t,x$t_se,config$delta_M)),pmax(pd,eqt))
    for(j in seq_along(classes)) {k <- k+1L;chunks[[k]] <- data.frame(result_row=ok,gene_id=x$gene_id,intervention=x$intervention,class_name=classes[j],direction=s,p_raw=pp[[j]],stringsAsFactors=FALSE)}
  }
  claims <- do.call(rbind,chunks);claims$adjusted_p_BY <- stats::p.adjust(claims$p_raw,"BY");claims$adjusted_p_BH <- stats::p.adjust(claims$p_raw,"BH")
  claims$raw_p <- claims$p_raw;claims$adjusted_p <- claims$adjusted_p_BY
  claims$family_id <- "classification_all_genes_T_classes_directions";claims$rejected <- claims$adjusted_p_BY<=config$alpha;claims$rejected_BH <- claims$adjusted_p_BH<=config$alpha
  # Diagnostic family has each retained gene only once per direction, no T duplication.
  unique_gene <- !duplicated(x$gene_id);xd <- x[unique_gene,,drop=FALSE]
  perturb <- do.call(rbind,lapply(c(1,-1),function(s)data.frame(gene_id=xd$gene_id,direction=s,p_raw=stats::pt((config$delta_D-s*xd$d)/xd$d_se,xd$df))))
  perturb$adjusted_p_BY <- stats::p.adjust(perturb$p_raw,"BY");perturb$supported <- perturb$adjusted_p_BY<=config$alpha
  perturb$family_id <- "perturbation_diagnostic_genes_directions"
  supported <- unique(perturb$gene_id[perturb$supported])
  z$perturbation_supported <- NA;z$perturbation_supported[ok] <- x$gene_id %in% supported
  z$classification <- NA_character_;z$direction <- NA_integer_;z$classification_adjusted_p <- NA_real_;z$development <- FALSE
  z$classification[ok] <- "insufficient_evidence";z$status[ok] <- "analyzed";z$reason[ok] <- "no_BY_class_claim_supported"
  un <- ok[!z$perturbation_supported[ok]];z$status[un] <- "unconfirmed_perturbation";z$classification[un] <- NA_character_;z$reason[un] <- "original_perturbation_not_supported_not_evidence_of_absence"
  hits <- claims[claims$rejected,,drop=FALSE];ct <- tabulate(hits$result_row,nbins=nrow(z));single <- hits[ct[hits$result_row]==1,,drop=FALSE]
  if(nrow(single)) {i <- single$result_row;z$status[i]<-"classified";z$classification[i]<-single$class_name;z$direction[i]<-single$direction;z$classification_adjusted_p[i]<-single$adjusted_p_BY;z$reason[i]<-"directional_IUT_global_BY_supported"}
  conflicts <- which(ct>1);z$status[conflicts]<-"conflict";z$classification[conflicts]<-NA_character_;z$reason[conflicts]<-"multiple_mutually_exclusive_claims_supported_requires_investigation"
  labels <- c(within_reference="参考范围内恢复",partial_reversal="部分回调",overshoot="越过参考范围",further_deviation="进一步偏离",no_meaningful_change="未发生有意义改变",insufficient_evidence="证据不足")
  z$classification_label <- unname(labels[z$classification]);z$classification_label[z$status=="unconfirmed_perturbation"] <- "原始扰动未获支持"
  result$results<-z;result$claims<-claims;result$perturbation_diagnostics<-perturb
  nd <- if(is.null(result$direct_comparisons))0L else nrow(result$direct_comparisons)
  result$families <- data.frame(family_id=c("classification_all_genes_T_classes_directions","perturbation_diagnostic_genes_directions","direct_all_genes_all_T_pairs"),n_hypotheses=c(nrow(claims),nrow(perturb),nd),adjustment="BY",alpha=config$alpha,scope=c("retained genes x all planned T x 5 classes x 2 directions","retained genes x 2 fixed directions; diagnostic only","retained genes x all unordered T pairs; two-sided zero-effect"))
  result$diagnostics$n_conflicts <- length(conflicts);result
}
