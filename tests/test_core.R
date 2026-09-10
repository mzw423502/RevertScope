# Deterministic, independent tests. Do not retune seeds to obtain positives.
set.seed(17041)
test_seed <- 17041L
ng <- 240L
samples_fixture <- data.frame(sample_id = paste0("s", 1:12),
  group = rep(c("C", "D", "T1", "T2"), each = 3),
  role = rep(c("C", "D", "T", "T"), each = 3),
  batch = rep(c("b1", "b2", "b1"), 4), stringsAsFactors = FALSE)
mu <- matrix(120, ng, 12)
mu[1:40, 4:6] <- 480
mu[1:40, 10:12] <- 240
mu[41:80, 4:12] <- 360
mu[81:120, 4:6] <- 30
mu[81:120, 10:12] <- 60
counts_fixture <- matrix(rnbinom(length(mu), mu = as.vector(mu), size = 30), ng, 12,
  dimnames = list(paste0("g", seq_len(ng)), samples_fixture$sample_id))
counts_fixture[ng, ] <- 0
fixture_dir <- file.path(getOption("revertscope.test_output", "logs"), "test_fixtures")
dir.create(fixture_dir, showWarnings = FALSE, recursive = TRUE)
counts_table <- data.frame(gene_id = rownames(counts_fixture), counts_fixture, check.names = FALSE)
write.table(counts_table, file.path(fixture_dir, "counts.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
write.csv(counts_table, file.path(fixture_dir, "counts.csv"), row.names = FALSE)
write.table(samples_fixture, file.path(fixture_dir, "samples.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
write.csv(samples_fixture, file.path(fixture_dir, "samples.csv"), row.names = FALSE)

cfg <- list(source_confirmed = TRUE, data_scale = "raw_counts", independent_replicates = TRUE,
  design_type = "independent", delta_D = 0.75, epsilon_R = 0.5, delta_M = 0.5, epsilon_0 = 0.25, alpha = 0.05,
  min_replicates = 3L, min_count = 10, min_total_count = 15)
valid <- function(x = counts_fixture, s = samples_fixture, c = cfg) validate_inputs(x, s, c)
check("valid_independent_fixed_batch_design", {
  x <- valid(); stopifnot(qr(x$design)$rank == ncol(x$design), length(x$T) == 2L)
})
check("csv_tsv_roundtrip_and_mapping", {
  a <- read_inputs(file.path(fixture_dir, "counts.tsv"), file.path(fixture_dir, "samples.tsv"), cfg)
  b <- read_inputs(file.path(fixture_dir, "counts.csv"), file.path(fixture_dir, "samples.csv"), cfg)
  stopifnot(identical(a$counts, b$counts), isTRUE(all.equal(a$counts, counts_fixture)))
  reverse <- valid(s = samples_fixture[12:1, ])
  stopifnot(identical(colnames(reverse$counts), rev(samples_fixture$sample_id)))
})
check("xlsx_count_and_metadata_roundtrip", {
  python <- Sys.which("python")
  stopifnot(nzchar(python))
  exit <- system2(python, c("tests/fixtures/build_xlsx.py", fixture_dir))
  stopifnot(exit == 0L)
  x <- read_inputs(file.path(fixture_dir, "counts.xlsx"), file.path(fixture_dir, "samples.xlsx"), cfg)
  stopifnot(isTRUE(all.equal(x$counts, counts_fixture)))
})
check("missing_input_path", expect_error(read_inputs(file.path(fixture_dir, "absent.tsv"), file.path(fixture_dir, "samples.tsv"), cfg), "FILE_MISSING"))
check("duplicate_gene_id", { x <- counts_fixture; rownames(x)[2] <- rownames(x)[1]; expect_error(valid(x), "DUPLICATE_ID") })
check("duplicate_count_sample_id", { x <- counts_fixture; colnames(x)[2] <- colnames(x)[1]; expect_error(valid(x), "DUPLICATE_ID") })
check("duplicate_metadata_sample_id", { s <- samples_fixture; s$sample_id[2] <- s$sample_id[1]; expect_error(valid(s = s), "SAMPLE_ID") })
check("sample_mapping_mismatch", { s <- samples_fixture; s$sample_id[1] <- "absent"; expect_error(valid(s = s), "SAMPLE_MAPPING") })
check("negative_count", { x <- counts_fixture; x[1, 1] <- -1; expect_error(valid(x), "NEGATIVE_COUNT") })
check("missing_count", { x <- counts_fixture; x[1, 1] <- NA_real_; expect_error(valid(x), "COUNT_MISSING") })
check("infinite_count", { x <- counts_fixture; x[1, 1] <- Inf; expect_error(valid(x), "COUNT_MISSING") })
check("noninteger_count_never_rounded", { x <- counts_fixture; x[1, 1] <- 1.5; expect_error(valid(x), "NONINTEGER_COUNT") })
check("zero_library", { x <- counts_fixture; x[, 1] <- 0; expect_error(valid(x), "ZERO_LIBRARY") })
check("source_confirmation_mandatory", { c <- cfg; c$source_confirmed <- FALSE; expect_error(valid(c = c), "SOURCE_CONFIRMATION") })
check("integer_tpm_still_rejected_by_source", { c <- cfg; c$data_scale <- "TPM"; expect_error(valid(c = c), "SOURCE_CONFIRMATION") })
check("paired_design_rejected", { c <- cfg; c$design_type <- "paired"; expect_error(valid(c = c), "UNSUPPORTED_DESIGN") })
check("independence_confirmation_mandatory", { c <- cfg; c$independent_replicates <- FALSE; expect_error(valid(c = c), "UNSUPPORTED_DESIGN") })
check("longitudinal_metadata_rejected", { s <- samples_fixture; s$timepoint <- 1:12; expect_error(valid(s = s), "UNSUPPORTED_DESIGN") })
check("repeated_biological_units_rejected", { s <- samples_fixture; s$biological_unit <- rep(1:6, 2); expect_error(valid(s = s), "NONINDEPENDENT_UNIT") })
check("group_role_error", { s <- samples_fixture; s$role[1] <- "T"; expect_error(valid(s = s), "GROUP_ROLE") })
check("insufficient_replicates", expect_error(valid(counts_fixture[, -1], samples_fixture[-1, ]), "REPLICATES"))
check("confounded_batch_not_estimable", { s <- samples_fixture; s$batch <- s$group; expect_error(valid(s = s), "DESIGN_NOT_ESTIMABLE") })
check("invalid_threshold_config", { c <- cfg; c$epsilon_R <- -1; expect_error(valid(c = c), "CONFIG") })
check("duplicate_gene_id_in_file", {
  x <- counts_table; x$gene_id[2] <- x$gene_id[1]
  write.table(x, file.path(fixture_dir, "duplicate.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(read_inputs(file.path(fixture_dir, "duplicate.tsv"), file.path(fixture_dir, "samples.tsv"), cfg), "GENE_ID")
})
check("fit_two_interventions", {
  fitted_test <<- fit_response(counts_fixture, samples_fixture, cfg)
  stopifnot(nrow(fitted_test$results) == ng * 2L, length(fitted_test$contrasts) == 2L,
    !any(fitted_test$results$development), all(c("cov_dt", "cov_dr", "cov_tr") %in% names(fitted_test$results)))
})
check("effect_and_covariance_identity", {
  z <- subset(fitted_test$results, status != "filtered")
  stopifnot(max(abs(z$r - z$d - z$t)) < 1e-8,
    max(abs(z$r_se^2 - z$d_se^2 - z$t_se^2 - 2 * z$cov_dt)) < 1e-8,
    max(abs(z$cov_dr - z$d_se^2 - z$cov_dt)) < 1e-8,
    max(abs(z$cov_tr - z$t_se^2 - z$cov_dt)) < 1e-8,
    all(z$d_lower <= z$d), all(z$d_upper >= z$d), all(z$df > 0))
})
check("low_expression_preserved_as_distinct_state", {
  z <- fitted_test$results[fitted_test$results$gene_id == paste0("g", ng), ]
  stopifnot(nrow(z) == 2L, all(z$status == "filtered"), all(is.na(z$d)),
    all(is.na(z$classification)), all(nzchar(z$reason)))
})
source('scripts/reference_methods.R')
key_claim <- function(z) paste(z$gene_id,z$intervention,z$class_name,z$direction,sep='|')
check('independent_IUT_matches_all_claims_full_family', {
  b <- reference_claims(fitted_test$results,fitted_test$config); a <- fitted_test$claims
  b <- b[match(key_claim(a),key_claim(b)),]
  stopifnot(nrow(a)==(ng-1)*2*5*2,!anyNA(b$gene_id),
    max(abs(a$p_raw-b$p_raw))<1e-14,max(abs(a$adjusted_p_BY-b$adjusted_p_BY))<1e-14,
    max(abs(a$adjusted_p_BH-b$adjusted_p_BH))<1e-14,
    identical(a$rejected,b$rejected),all(a$adjusted_p_BY>=a$adjusted_p_BH))
})
manual <- data.frame(gene_id=paste0('m',1:6),intervention='T',d=2,t=c(-2,-1,-3,1,0,-.4),
  d_se=.001,t_se=.001,r_se=.001,df=1000,status='analyzed')
manual$r <- manual$d+manual$t
manual_obj <- list(schema_version='2.0.0',results=manual,config=cfg)
expected <- c('within_reference','partial_reversal','overshoot','further_deviation','no_meaningful_change','insufficient_evidence')
check('five_supported_categories_and_unassigned_gap',stopifnot(identical(classify_response(manual_obj)$results$classification,expected)))
check('positive_negative_mirror_inference', {
  b <- manual_obj; for(nm in c('d','t','r'))b$results[[nm]] <- -b$results[[nm]]
  a <- classify_response(manual_obj); b <- classify_response(b)
  stopifnot(identical(b$results$classification,expected),identical(a$results$direction,-b$results$direction))
})
# Direct inequalities are the independent oracle; uncertain is not a truth class.
truth_membership <- function(d,t,c=cfg) {
  r <- d+t
  do.call(cbind,lapply(c(1,-1),function(s)cbind(s*d>c$delta_D & abs(r)<c$epsilon_R & -s*t>c$delta_M,
    s*d>c$delta_D & s*r>c$epsilon_R & -s*t>c$delta_M,
    s*d>c$delta_D & s*r< -c$epsilon_R & -s*t>c$delta_M,
    s*d>c$delta_D & s*t>c$delta_M,
    s*d>c$delta_D & abs(t)<c$epsilon_0)))
}
check('truth_regions_mutually_exclusive_including_boundaries', {
  grid <- expand.grid(d=seq(-3,3,by=.125),t=seq(-4,4,by=.125))
  stopifnot(all(rowSums(truth_membership(grid$d,grid$t))<=1))
  # Exact r boundaries and t boundaries are neither adjacent response.
  stopifnot(all(rowSums(truth_membership(c(2,2,2,2,.75),c(-1.5,-2.5,-.5,.25,-.75)))==0))
})
check('exact_boundaries_cannot_produce_directional_claim', {
  b <- manual_obj;b$results <- b$results[rep(1,5),];b$results$gene_id<-paste0('bd',1:5)
  b$results$d<-c(2,2,2,2,.75);b$results$t<-c(-1.5,-2.5,-.5,.25,-.75);b$results$r<-b$results$d+b$results$t
  z <- classify_response(b);stopifnot(!any(z$claims$rejected),!any(z$results$status=='classified'))
})
check('illegal_threshold_order_rejected', {
  for(change in list(c(delta_D=.5),c(epsilon_R=.8),c(delta_M=.25),c(epsilon_0=.5))) {
    c <- cfg;for(nm in names(change))c[[nm]]<-change[[nm]]
    expect_error(valid(c=c),'THRESHOLD_ORDER')
  }
})
check('legacy_schema_rejected_explicitly', {b<-manual_obj;b$schema_version<-'1.0';expect_error(classify_response(b),'SCHEMA_VERSION')})
check('old_threshold_names_rejected', {c<-cfg;c$reference_margin<-.5;expect_error(valid(c=c),'CONFIG')})
check('alpha_change_requires_refit', {c<-cfg;c$alpha<-.1;expect_error(classify_response(manual_obj,c),'REFIT_REQUIRED')})
check('no_perturbation_is_distinct_from_failed_intervention', {
  b<-manual_obj;b$results$d<-0;b$results$r<-b$results$t
  z<-classify_response(b)$results;stopifnot(all(z$status=='unconfirmed_perturbation'),all(is.na(z$classification)),all(nzchar(z$reason)))
})
check('row_and_sample_order_invariant_effects_claims', {
  revfit<-fit_response(counts_fixture[ng:1,12:1],samples_fixture[12:1,],cfg)
  a<-fitted_test$results;b<-revfit$results
  b<-b[match(paste(a$gene_id,a$intervention),paste(b$gene_id,b$intervention)),]
  stopifnot(max(abs(a$d-b$d),na.rm=TRUE)<1e-10,max(abs(a$t_se-b$t_se),na.rm=TRUE)<1e-10,identical(a$classification,b$classification))
  bc<-revfit$claims[match(key_claim(fitted_test$claims),key_claim(revfit$claims)),]
  stopifnot(max(abs(fitted_test$claims$adjusted_p_BY-bc$adjusted_p_BY))<1e-10)
})
check('C_D_role_exchange_obeys_contrast_algebra', {
  s<-samples_fixture;s$role[samples_fixture$role=='C']<-'D';s$role[samples_fixture$role=='D']<-'C'
  b<-fit_response(counts_fixture,s,cfg)$results;a<-fitted_test$results
  b<-b[match(paste(a$gene_id,a$intervention),paste(b$gene_id,b$intervention)),]
  stopifnot(max(abs(b$d+a$d),na.rm=TRUE)<1e-10,max(abs(b$t-a$r),na.rm=TRUE)<1e-10,max(abs(b$r-a$t),na.rm=TRUE)<1e-10)
})
check('direct_TA_TB_uses_shared_model_and_own_family', {
  a<-subset(fitted_test$results,intervention=='T1' & status!='filtered');b<-subset(fitted_test$results,intervention=='T2' & status!='filtered')
  q<-fitted_test$direct_comparisons
  stopifnot(nrow(q)==ng-1,max(abs(q$effect-(a$t-b$t)))<1e-10,all(q$se>0),
    max(abs(q$adjusted_p-p.adjust(q$raw_p,'BY')))<1e-14,
    all(q$family_id=='direct_all_genes_all_T_pairs'))
})
check('IUT_uses_unadjusted_component_maximum', {
  z<-manual_obj$results[1,];c<-cfg
  oracle<-max(pt((c$delta_D-z$d)/z$d_se,z$df),pt((c$delta_M+z$t)/z$t_se,z$df),
    pt((-c$epsilon_R-z$r)/z$r_se,z$df),pt((z$r-c$epsilon_R)/z$r_se,z$df))
  p<-subset(classify_response(manual_obj)$claims,gene_id=='m1' & class_name=='within_reference' & direction==1)$p_raw
  stopifnot(identical(as.numeric(p),as.numeric(oracle)))
})
