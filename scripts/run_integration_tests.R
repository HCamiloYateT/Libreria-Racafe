#!/usr/bin/env Rscript

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Se requiere testthat para ejecutar pruebas de integración.", call. = FALSE)
}

testthat::test_file(
  "tests/integration/test-e2e.R",
  stop_on_failure = TRUE,
  stop_on_warning = TRUE
)
