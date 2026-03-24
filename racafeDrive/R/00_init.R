# ============================================================
# racafeDrive — Inicializacion del paquete
# ============================================================

# Cache en memoria del token Microsoft Graph (por proceso, por sesion R)
.token_cache <- new.env(parent = emptyenv())

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
