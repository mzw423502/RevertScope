# Third-party components and public data

RevertScope calls R and Bioconductor packages installed independently from their official repositories. Exact package versions, source URLs, hashes and declared licenses are recorded in `renv.lock`, `config/dependency_sources.tsv` and `config/third_party_components.tsv`. No compiled R package library, R runtime, external JavaScript distribution, font file or external binary is committed here.

The small files under `examples/` are simulated. Public-study raw count files are not committed. The retrieval script and mappings point to official NCBI GEO records; those records and their associated publications retain their own terms.
