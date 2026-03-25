#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Uso: Rscript scripts/coverage_gate.R <min_cov> <pkg1> [pkg2 ...]", call. = FALSE)
}

min_cov <- as.numeric(args[[1]])
if (is.na(min_cov) || min_cov <= 0 || min_cov > 100) {
  stop("`min_cov` debe estar en (0, 100].", call. = FALSE)
}

pkgs <- args[-1]

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Se requiere el paquete 'covr'.", call. = FALSE)
}

medias <- vapply(pkgs, function(pkg) {
  cov <- covr::package_coverage(path = pkg, quiet = FALSE)
  as.numeric(covr::percent_coverage(cov))
}, numeric(1))

names(medias) <- pkgs

cat("Cobertura por paquete (%):\n")
print(round(medias, 2))

promedio <- mean(medias)
cat(sprintf("Cobertura promedio: %.2f%%\n", promedio))

if (any(medias < min_cov)) {
  bajos <- names(medias)[medias < min_cov]
  stop(
    sprintf(
      "Cobertura por debajo del umbral %.2f%% en: %s",
      min_cov,
      paste(bajos, collapse = ", ")
    ),
    call. = FALSE
  )
}

baseline_file <- Sys.getenv("BASELINE_COVERAGE_FILE", unset = "")
if (nzchar(baseline_file) && file.exists(baseline_file)) {
  baseline <- as.numeric(readLines(baseline_file, warn = FALSE)[1])
  if (!is.na(baseline)) {
    cat(sprintf("Cobertura baseline PR: %.2f%%\n", baseline))
    if (promedio + 1e-8 < baseline) {
      stop(
        sprintf(
          "La cobertura promedio bajó de %.2f%% a %.2f%%.",
          baseline,
          promedio
        ),
        call. = FALSE
      )
    }
  }
}

cat("Quality gate de cobertura superado.\n")
