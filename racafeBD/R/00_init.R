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

# Helper interno: error homologado para integraciones de BD
.error_bd <- function(causa, accion, funcion = NULL) {
  contexto <- if (!is.null(funcion)) sprintf("%s - %s", funcion, causa) else causa
  stop(
    sprintf("[racafeBD] %s. Accion sugerida: %s.", contexto, accion),
    call. = FALSE
  )
}
