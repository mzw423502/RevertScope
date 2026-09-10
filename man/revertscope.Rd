\name{revertscope}
\alias{read_inputs}
\alias{validate_inputs}
\alias{fit_response}
\alias{classify_response}
\alias{export_response}
\alias{plot_response}
\title{Transcriptomic intervention response analysis}
\description{Analyze raw gene counts from independent biological replicates using a unified limma-voom model. Directional intersection-union tests use a prespecified BY-adjusted family. Validation scope and assumptions are documented in the project methods.}
\usage{
read_inputs(counts_path, samples_path, config)
validate_inputs(counts, samples, config)
fit_response(counts, samples, config)
classify_response(result, config = result$config)
export_response(object, out)
plot_response(object, intervention = NULL)
}
\arguments{
\item{counts_path}{CSV, TSV or XLSX file with gene_id followed by sample columns.}
\item{samples_path}{Metadata file with sample_id, group and role columns; optional batch.}
\item{config}{Named configuration list or YAML path, including raw-count source and independent-replicate confirmations and log2 thresholds delta_D, epsilon_R, delta_M, epsilon_0.}
\item{counts}{Nonnegative integer count matrix with unique gene and sample IDs.}
\item{samples}{Sample metadata data frame.}
\item{result, object}{A schema 2.0.0 revertscope_result returned by fit_response; older schemas require refitting.}
\item{out}{Output directory.}
\item{intervention}{Intervention group to display; defaults to the first group.}
}
\value{Input functions return validated input lists; fitting and classification return a revertscope_result. Export returns the object invisibly; plotting returns the displayed rows invisibly.}
\details{Roles are C (reference), D (perturbed), and T (intervention on D background). One or more intervention groups are supported. Correlated, paired, longitudinal and single-cell designs are outside scope. Effects are d=D-C, t=T-D and r=T-C. QR-based gene-specific covariance is conditional on fitted voom weights and empirical-Bayes variance. See docs/METHODS.md in the project for interval and classification definitions.}
