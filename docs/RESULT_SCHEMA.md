# RevertScope canonical result schema 2.0.0

`result.rds` is one `revertscope_result` list with `schema_version="2.0.0"` and `method_version="0.2.0"`. Export, plotting and classification reject an older or absent schema explicitly. Refit original counts with the four-threshold configuration; do not transform intervals from an older schema into new claims. The exporter checks required members before creating output.

|Member / TSV|Contract|
|---|---|
|results|One row for every input gene × planned intervention, including filtered rows; gene_id, intervention, comparison_d/t/r, d/t/r, *_se, *_lower/upper, *_p/padj, cov_dt/dr/tr, df, identity errors, status, reason, classification, classification_label, direction, classification_adjusted_p, perturbation_supported, development|
|claims|All retained genes × all planned T × five class_name values × two direction values (+1/−1). result_row is a one-based results row key. p_raw/adjusted_p_BY/adjusted_p_BH, rejected/rejected_BH and family_id are preserved. raw_p and adjusted_p are aliases of p_raw and adjusted_p_BY.|
|perturbation_diagnostics|Each retained gene × two directions once, without T duplication; p_raw, adjusted_p_BY, supported, family_id. Diagnostic support never selects the main class family.|
|direct_comparisons|Each retained gene × unordered T pair, lexical T order fixes contrast orientation. intervention_a/b, comparison, effect, se, lower, upper, df, raw_p, adjusted_p, family_id. No T pair produces a zero-row table with headers.|
|families|family_id, n_hypotheses, adjustment, alpha, scope. Main class, diagnostic perturbation and direct comparison are separate BY families.|
|sample_mapping|Fitted sample_id/group/role and supplied metadata, in fitted sample order; no invented batch or biological identity.|
|config|Frozen source confirmations, filtering, thresholds delta_D/epsilon_R/delta_M/epsilon_0, alpha and other run settings.|
|design / contrasts|Actual design matrix and named per-T d/t/r contrast matrices.|
|filter|Input gene_id and retained indicator.|
|fit_inputs|Actual voom expression and weights; coefficients, sigma2, df_residual, df_prior, s2_prior, s2_post, df_total. posterior_variance aliases s2_post for prior code.|
|diagnostics|Input/retained counts, samples, design rank, residual df, identity error, normalization, interval and error-control labels, metadata handling, sample order, library sizes, normalization factors, software versions, n_conflicts.|
|provenance|CLI-attached input paths and SHA-256, command/run metadata as available. The fit API itself does not invent file paths for in-memory inputs.|

Valid result statuses distinguish `filtered`, `unconfirmed_perturbation`, `analyzed` with `insufficient_evidence`, `classified`, and `conflict`. Unsupported/nonestimable designs stop with actionable typed errors; they are not fabricated gene rows. Filtered and original-perturbation-unconfirmed rows have classification NA. Multiple main claims for one gene/T produce conflict, not significance-based selection. The fixed class names are `within_reference`, `partial_reversal`, `overshoot`, `further_deviation`, `no_meaningful_change`; insufficient evidence is an inferential fallback, not a sixth generated biological truth.

All effect intervals are **95% ordinary marginal** moderated-t intervals. Main class decisions are BY-adjusted directional IUT claims. These are different inferential objects. Per-contrast `*_p` and `*_padj` are descriptive two-sided zero-effect P and BH output and do not select class claims. `adjusted_p` is not an individual classification-error probability. The three BY families do not jointly imply one overall error guarantee. BH is sensitivity output on the same full class family.

The exporter writes RDS first, then derives TSV, summary, candidates, plots and report from that exact object. `candidates.tsv` contains only classified primary-BY within_reference or partial_reversal rows. Summary preserves independent statuses, including missing class names. UI filters are views; never recompute multiplicity after selecting genes or arms. `run_manifest.json` records schema/method, canonical RDS SHA-256, config, supplied provenance, diagnostics, family definitions, table dimensions/columns, sample mapping and map file-to-intervention mapping. PNG (300 dpi), SVG and PDF are views of the same estimates; the reference band uses epsilon_R and cannot itself prove equivalence.
