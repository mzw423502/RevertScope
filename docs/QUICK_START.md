# RevertScope quick start

## Requirements

- Windows 11 x64 is the validated platform for version 1.0.0.
- R 4.6.1 and Bioconductor 3.23 were used for the frozen release.
- Initial dependency restoration may require a network connection; completed analyses run locally.
- Python is needed only for XLSX test-fixture generation and optional public-data retrieval, not for routine CSV/TSV analysis.

## Restore dependencies

From the repository root:

```powershell
pwsh -File scripts/restore_release.ps1 -Rscript "C:\path\to\Rscript.exe"
```

The script verifies source-archive hashes from `config/dependency_sources.tsv` and installs packages into the repository-local `.library` directory.

## Run the fixed simulated example

```powershell
Rscript scripts/run_analysis.R --counts examples/counts.tsv --samples examples/samples.tsv --config config/analysis.yml --out results/example
```

Expected outputs include `result.rds`, full and candidate tables, summary tables, a run manifest, session information, an HTML/Markdown report, and response-map files. All are derived from one canonical result object.

## Start the local application

```powershell
Rscript scripts/run_app.R
```

On Windows, `启动RevertScope.cmd` performs the same launch through the repository helper scripts. The application listens on `127.0.0.1`; close its console to stop it.

## Run tests

```powershell
Rscript scripts/test_all.R --out logs
```

The suite writes `logs/test_results.json` with individual statuses, inputs, parameters, seed and session information. The release expectation is 43 passed and 0 failed in the validated environment.

## Retrieve the GSE208041 public example

```powershell
python scripts/fetch_release_public.py
Rscript scripts/prepare_release_examples.R
```

The Python script downloads from official NCBI GEO endpoints, checks fixed SHA-256 hashes, confirms the 12-column sample mapping, and writes adapted counts and metadata under `data/public/GSE208041`. Public raw files are not part of the repository.

## Input reminders

- Confirm from source records that the input is raw gene counts.
- Keep real batch variables and biological-unit information in the sample table.
- Do not round TPM, FPKM or log-expression values into counts.
- Do not run paired, longitudinal, repeated-subject or single-cell designs as independent samples.
- Treat `unconfirmed_perturbation`, `insufficient_evidence` and `filtered` as distinct states.
