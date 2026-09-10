#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(script)) dirname(dirname(normalizePath(script[1], mustWork = TRUE))) else getwd()
setwd(root)
.libPaths(unique(c(file.path(root, ".library"), .libPaths())))
test_args <- commandArgs(trailingOnly = TRUE)
out_at <- match("--out", test_args)
test_out <- if (!is.na(out_at) && out_at < length(test_args)) test_args[out_at + 1L] else "logs"
dir.create(test_out, showWarnings = FALSE, recursive = TRUE)
options(revertscope.test_output = test_out)
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install the locked jsonlite dependency before testing.")
records <- list()
check <- function(name, code) {
  start <- Sys.time()
  problem <- tryCatch({ force(code); NULL }, error = function(e) conditionMessage(e))
  records[[length(records) + 1L]] <<- list(name = name, status = if (is.null(problem)) "PASS" else "FAIL",
    elapsed_seconds = as.numeric(difftime(Sys.time(), start, units = "secs")), message = problem)
  cat(if (is.null(problem)) "PASS" else "FAIL", name, if (!is.null(problem)) paste0(": ", problem) else "", "\n")
  invisible(is.null(problem))
}
skip <- function(name, reason) { records[[length(records) + 1L]] <<- list(name = name, status = "SKIP", elapsed_seconds = 0, message = reason); cat("SKIP", name, ":", reason, "\n") }
expect_error <- function(expr, pattern = NULL) {
  msg <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (is.null(msg)) stop("Expected an informative error but expression succeeded")
  if (!is.null(pattern) && !grepl(pattern, msg, ignore.case = TRUE)) stop(paste("Error did not match", pattern, ":", msg))
  if (nchar(msg) < 12L) stop("Error message is too short to explain correction")
  invisible(msg)
}
check("source_core", source("R/core.R", local = .GlobalEnv))
check("run_test_definitions", source("tests/test_core.R", local = .GlobalEnv))
failed <- sum(vapply(records, function(x) identical(x$status, "FAIL"), logical(1)))
report <- list(schema_version = "2.0.0", executed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  command = paste(commandArgs(), collapse = " "), exit_code = as.integer(failed > 0L),
  passed = sum(vapply(records, function(x) identical(x$status, "PASS"), logical(1))), failed = failed,
  skipped = sum(vapply(records, function(x) identical(x$status, "SKIP"), logical(1))), tests = records,
  random_seed = if (exists("test_seed")) test_seed else NULL,
  parameters = if (exists("cfg")) cfg else NULL,
  test_inputs_sha256 = setNames(lapply(c("R/core.R", "tests/test_core.R", "scripts/test_all.R", "scripts/reference_methods.R", "tests/fixtures/build_xlsx.py"),function(p)digest::digest(file=p,algo="sha256")),c("R/core.R", "tests/test_core.R", "scripts/test_all.R", "scripts/reference_methods.R", "tests/fixtures/build_xlsx.py")),
  session_info = capture.output(sessionInfo()))
jsonlite::write_json(report, file.path(test_out,"test_results.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
cat(sprintf("Tests: %d passed, %d failed. Evidence: %s\n", report$passed, failed, file.path(test_out,"test_results.json")))
quit(status = as.integer(failed > 0L))


