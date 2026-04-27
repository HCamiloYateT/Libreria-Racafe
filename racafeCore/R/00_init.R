# ============================================================
# racafeCore — Inicializacion del paquete
# Colores corporativos y helpers internos
# ============================================================

# Colores corporativos Racafe
.COLORES_RACAFE <- c(
  "#28B78D", "#1A7A5E", "#85D4BE", "#0D4F3C",
  "#5BC8A8", "#3DA882", "#A8E6D4", "#145C46",
  "#6ECFB5", "#2E9E74"
)

# ---- Helper interno compartido por todo el ecosistema ----
# No exportado: cada paquete hijo lo copia en su propio 00_init.R
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
