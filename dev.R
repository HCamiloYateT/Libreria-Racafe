#!/usr/bin/env Rscript
# ============================================================
# dev.R — Script de desarrollo local del monorepo Racafe
# Ejecutar desde la raiz del repositorio: Rscript dev.R
# ============================================================

# ---- Dependencias del script ----
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("withr",    quietly = TRUE)) install.packages("withr")

# Orden de instalacion: respeta el grafo de dependencias
PAQUETES <- c(
  "racafeCore",     # sin dependencias internas
  "racafeBD",       # depende de racafeCore
  "racafeDrive",    # depende de racafeCore
  "racafeGraph",    # depende de racafeCore
  "racafeShiny",    # depende de racafeCore + racafeGraph
  "racafeForecast"  # depende de racafeCore
)

# ---- Subcomandos ----
args <- commandArgs(trailingOnly = TRUE)
cmd  <- if (length(args) == 0) "install" else args[1]
pkg  <- if (length(args) >= 2) args[2]   else NULL

# Instalar un solo paquete o todos
instalar <- function(paquetes = PAQUETES) {
  for (p in paquetes) {
    if (!dir.exists(p)) {
      message(sprintf("ADVERTENCIA: carpeta '%s' no encontrada, saltando.", p))
      next
    }
    message(sprintf("\n>>> Instalando %s ...", p))
    devtools::install(p, dependencies = TRUE, upgrade = "never",
                      quiet = FALSE)
    message(sprintf("    %s OK", p))
  }
  invisible(NULL)
}

# Correr tests de un paquete o todos
testear <- function(paquetes = PAQUETES) {
  resultados <- list()
  for (p in paquetes) {
    if (!dir.exists(p)) next
    message(sprintf("\n>>> Tests: %s", p))
    res <- devtools::test(p)
    resultados[[p]] <- res
  }
  invisible(resultados)
}

# Regenerar documentacion de un paquete o todos
documentar <- function(paquetes = PAQUETES) {
  for (p in paquetes) {
    if (!dir.exists(p)) next
    message(sprintf("\n>>> Document: %s", p))
    devtools::document(p)
  }
  invisible(NULL)
}

# Verificar (R CMD check) un paquete o todos
verificar <- function(paquetes = PAQUETES) {
  for (p in paquetes) {
    if (!dir.exists(p)) next
    message(sprintf("\n>>> Check: %s", p))
    devtools::check(p, error_on = "error")
  }
  invisible(NULL)
}

# Cobertura de tests de un paquete
cobertura <- function(paquete) {
  if (!requireNamespace("covr", quietly = TRUE)) {
    stop("Instalar covr: install.packages('covr')")
  }
  cov <- covr::package_coverage(paquete)
  covr::report(cov)
  invisible(cov)
}

# ---- Despacho de comandos ----
objetivo <- if (!is.null(pkg)) pkg else PAQUETES

switch(
  cmd,
  install  = instalar(objetivo),
  test     = testear(objetivo),
  document = documentar(objetivo),
  check    = verificar(objetivo),
  coverage = {
    if (is.null(pkg)) stop("Uso: Rscript dev.R coverage <paquete>")
    cobertura(pkg)
  },
  {
    cat("
Uso: Rscript dev.R <comando> [paquete]

Comandos:
  install           Instala todos los paquetes en orden correcto
  install <pkg>     Instala solo el paquete especificado
  test              Corre tests de todos los paquetes
  test <pkg>        Corre tests solo del paquete especificado
  document          Regenera documentacion de todos los paquetes
  document <pkg>    Regenera documentacion del paquete especificado
  check             R CMD check en todos los paquetes
  check <pkg>       R CMD check del paquete especificado
  coverage <pkg>    Reporte de cobertura de tests del paquete

Paquetes disponibles:
  racafeCore | racafeBD | racafeDrive | racafeGraph | racafeShiny | racafeForecast

Ejemplos:
  Rscript dev.R install
  Rscript dev.R install racafeCore
  Rscript dev.R test    racafeGraph
  Rscript dev.R coverage racafeShiny
")
  }
)
