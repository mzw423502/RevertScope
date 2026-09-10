# RevertScope methods

Software version 1.0.0 uses method version 0.2.0 and result schema 2.0.0. The implementation supports independent-replicate bulk RNA-seq gene-count studies with one reference group (C), one perturbation group (D), and one or more intervention groups on the D background (T).

## Input contract and validation

The count table is CSV, TSV or XLSX with a unique `gene_id` in the first column and one non-negative integer count column per sample. The sample table contains `sample_id`, `group` and `role`; an optional `batch` column is treated as a fixed effect. Counts and samples must match one to one. Empty or duplicated identifiers, missing or non-integer counts, negative counts, zero libraries, inadequate replication, rank-deficient designs and batch terms confounded with requested contrasts are rejected with structured error codes.

The configuration must explicitly confirm `data_scale: raw_counts`, `source_confirmed: true`, `independent_replicates: true`, and `design_type: independent`. Integer-valued appearance alone is not accepted as evidence that a table contains source counts. TPM, FPKM and log-transformed expression values are not converted into counts. Paired, longitudinal, repeated-subject, single-cell and mixed-effect designs are outside the current scope.

## Filtering, normalization and model

`edgeR::filterByExpr` filters low-expression genes using the supplied design; filtered genes remain in the full output with `status = filtered` and a reason code. Library sizes are reset after filtering, TMM normalization factors are calculated, and `limma::voom` produces log2-CPM values and observation weights. A no-intercept design contains group coefficients and any estimable fixed batch term. `limma::lmFit` and `limma::eBayes(trend = FALSE, robust = FALSE)` fit one model for all planned contrasts.

For each retained gene and intervention:

```text
d = D - C
t = T - D
r = T - C = d + t
```

Effects are log2 contrasts. RevertScope calculates per-gene contrast covariance from the weighted design by column-pivoted QR decomposition. The production checks require `r = d + t` and `Var(r) = Var(d) + Var(t) + 2 Cov(d,t)` within a numerical tolerance of `1e-8`. Stored intervals are ordinary 95% marginal moderated-t intervals.

## Response classes

Four prespecified log2-scale margins define the response regions: `delta_D` for the minimum original perturbation, `epsilon_R` for residual reference tolerance, `delta_M` for a meaningful intervention-associated movement, and `epsilon_0` for no meaningful change. Defaults are 0.75, 0.50, 0.50 and 0.25, with `alpha = 0.05`. They are analysis settings rather than universal biological cut-offs and should be fixed before inspecting the classifications.

For each prespecified perturbation direction `s` in `{+1, -1}`, every response statement requires `s*d > delta_D`. Additional conditions are:

| Class | Additional conditions |
|---|---|
| `within_reference` | `-epsilon_R < r < epsilon_R` and `-s*t > delta_M` |
| `partial_reversal` | `s*r > epsilon_R` and `-s*t > delta_M` |
| `overshoot` | `s*r < -epsilon_R` and `-s*t > delta_M` |
| `further_deviation` | `s*t > delta_M` |
| `no_meaningful_change` | `-epsilon_0 < t < epsilon_0` |

Each class is tested as an intersection-union test. Its raw P value is the maximum of the one-sided component P values required for the class. The primary family contains every retained gene, planned intervention, class and direction and is adjusted once with the Benjamini-Yekutieli procedure. Benjamini-Hochberg adjusted values are preserved as an explicitly exploratory view of the same family.

Original-perturbation support is summarized in a separate BY-adjusted diagnostic family. A gene without supported original perturbation receives `unconfirmed_perturbation`; this is not evidence of no perturbation. A gene with supported perturbation but without a supported response class receives `insufficient_evidence`; this is not evidence that the intervention had no effect. A formal `no_meaningful_change` statement requires equivalence evidence and is not assigned from a non-significant zero-effect test.

## Multiple interventions

For every unordered intervention pair, the same fitted model estimates the direct `T_A - T_B` contrast, its standard error, marginal interval and P value. These comparisons form a separate BY-adjusted family. A significant response for one intervention and a non-significant response for another is not used as a substitute for the direct contrast.

## Result object and exports

The canonical `revertscope_result` object stores the full gene-by-intervention table, class hypotheses, perturbation diagnostics, direct intervention comparisons, sample mapping, configuration, design and contrasts, filtering state, voom expression and weights, model coefficients, posterior variances, degrees of freedom, covariance fields and version diagnostics. The command line and Shiny application use the same core. Tables, candidate lists, response maps, interval plots, standalone HTML reports and reopenable project bundles are derived from this object.

The response map places `d` on the horizontal axis and `t` on the vertical axis. The horizontal line is `t = 0`; the diagonal `t = -d` is equivalent to `r = 0`; the shaded diagonal region represents `-epsilon_R <= r <= epsilon_R`. A point estimate inside the shaded region does not by itself establish an equivalence-based response class.

## Reproducibility

Runs record input SHA-256 hashes, parameters, random seed, package versions, result dimensions and the canonical RDS hash. Dependency versions are locked in `renv.lock`. The fixed examples are simulated. Raw public-study files are retrieved from official repositories and verified against recorded hashes rather than committed to this source repository.
