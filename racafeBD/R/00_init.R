# ============================================================
# racafeBD — Inicializacion del paquete
# ============================================================

# Helper interno: verificar disponibilidad de paquete opcional
.check_pkg <- function(pkg, modulo = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "El paquete '%s' es requerido%s.\nInstalar con: install.packages('%s')",
      pkg,
      if (!is.null(modulo)) sprintf(" para '%s'", modulo) else "",
      pkg
    ), call. = FALSE)
  }
}
