# RevertScope

RevertScope is a local R/Shiny and command-line workflow for classifying transcriptomic perturbation-intervention responses from bulk RNA-seq gene-count matrices with independent biological replicates.

Users assign a reference group (C), a perturbation group (D), and one or more intervention groups on the D background (T). A unified limma-voom model estimates the linked effects `d = D - C`, `t = T - D`, and `r = T - C`, while retaining their covariance and checking `r = d + t`. Prespecified directional and equivalence tests support response classes such as recovery within reference, partial reversal, overshoot, further deviation, no meaningful change, and evidence-aware unresolved states.

## Scope

- Input: raw non-negative integer bulk RNA-seq gene counts and a sample-information table.
- Design: independent biological replicates, one C group, one D group, one or more T groups, and identifiable fixed batch effects.
- Interfaces: command line and a local Shiny application.
- Outputs: full result tables, candidates, response maps, effect intervals, HTML reports, and reopenable project bundles.
- Current exclusions: paired, longitudinal, single-cell, repeated-subject and mixed-effect designs; TPM, FPKM and log-transformed expression values are not accepted as counts.

## Installation

The frozen release was validated with R 4.6.1 and Bioconductor 3.23. Dependency versions and official source archives are recorded in `renv.lock` and `config/dependency_sources.tsv`.

On Windows, restore dependencies with:

```powershell
pwsh -File scripts/restore_release.ps1 -Rscript "C:\path\to\Rscript.exe"
```

The restore step may use the network. Analysis and the Shiny application run locally after the dependencies are installed; no cloud service is required.

## Command-line example

```powershell
Rscript scripts/run_analysis.R --counts examples/counts.tsv --samples examples/samples.tsv --config config/analysis.yml --out results/example
```

The fixed example is simulated and contains 2,101 genes and 18 samples. Outputs are written below the selected output directory.

## Start the application

On Windows, double-click `启动RevertScope.cmd`, or run:

```powershell
Rscript scripts/run_app.R
```

The application listens on the local machine. Closing its console stops the instance.

## Tests

```powershell
Rscript scripts/test_all.R --out logs
```

The release test suite contains 43 checks covering input validation, design estimability, model identities, response classification, exports, projects and key error paths. XLSX fixture generation requires Python with `openpyxl`; routine analysis does not require Python.

## Public datasets

The repository does not redistribute third-party raw study files. `scripts/fetch_release_public.py` downloads the GSE208041 count matrix and sample metadata from official NCBI GEO endpoints, verifies recorded SHA-256 hashes, and constructs the analysis mapping. Sample mappings and configurations for GSE208041 and GSE162699 are retained under `public_case_materials/`.

- GSE208041: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE208041
- GSE162699: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE162699

## Documentation

- [Quick start](docs/QUICK_START.md)
- [Methods](docs/METHODS.md)
- [Result schema](docs/RESULT_SCHEMA.md)
- [Chinese user guide](docs/user_guide_zh.md)

## Reproducibility and versioning

Software version: 1.0.0; method version: 0.2.0; result schema: 2.0.0. Random seeds and analysis parameters are recorded in run manifests. The repository contains source code and small simulated examples; the manuscript's frozen benchmark tables and derived public-case result bundle are distributed separately with the submission package.

## Citation

The accompanying manuscript is titled *RevertScope: an interactive and reproducible workflow for classifying transcriptomic perturbation-intervention responses*. A journal citation and DOI will be added after publication.

## License

RevertScope source code is released under the [MIT License](LICENSE). Third-party R packages and data sources retain their own licenses and are not relicensed by this repository.
