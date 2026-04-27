# ============================================================
# racafeShiny — Inicializacion del paquete
# ============================================================

# Registro de formatos personalizados (environment por proceso)
.formatos_racafe <- new.env(parent = emptyenv())

# Formatos por defecto al cargar el paquete
.onLoad <- function(libname, pkgname) {
  DefinirFormato("coma")
  DefinirFormato("numero")
  DefinirFormato("dinero")
  DefinirFormato("dolares")
  DefinirFormato("miles")
  DefinirFormato("porcentaje")
  DefinirFormato("cientifico")
  DefinirFormato("millones")
  DefinirFormato("entero")
  DefinirFormato("tiempo")
  DefinirFormato("kwh")
  DefinirFormato("log")

  # Alias retrocompatibles
  DefinirFormato("decimal", scales::label_number(
    accuracy = 0.01,
    big.mark = ","
  ))
  DefinirFormato("variacion", scales::label_number(
    accuracy = 0.01,
    scale = 100,
    suffix = "%",
    style_positive = "plus",
    big.mark = ","
  ))
}

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
