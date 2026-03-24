# ============================================================
# racafeCore — Inicializacion del paquete
# Registro de formatos, colores corporativos y helpers internos
# ============================================================

# Registro de formatos personalizados (environment por proceso)
.formatos_racafe <- new.env(parent = emptyenv())

# Colores corporativos Racafe
.COLORES_RACAFE <- c(
  "#28B78D", "#1A7A5E", "#85D4BE", "#0D4F3C",
  "#5BC8A8", "#3DA882", "#A8E6D4", "#145C46",
  "#6ECFB5", "#2E9E74"
)

# ---- Formatos por defecto al cargar el paquete ----
.onLoad <- function(libname, pkgname) {
  assign("numero",     scales::number_format(
    big.mark = ".", decimal.mark = ",", accuracy = 1
  ), envir = .formatos_racafe)
  assign("decimal",    scales::number_format(
    big.mark = ".", decimal.mark = ",", accuracy = 0.01
  ), envir = .formatos_racafe)
  assign("dinero",     scales::dollar_format(
    prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 1
  ), envir = .formatos_racafe)
  assign("porcentaje", scales::percent_format(
    accuracy = 0.1, decimal.mark = ",", big.mark = "."
  ), envir = .formatos_racafe)
  assign("variacion",  scales::percent_format(
    accuracy = 0.1, decimal.mark = ",", big.mark = ".",
    style_positive = "plus"
  ), envir = .formatos_racafe)
}

# ---- API publica del registro de formatos ----

#' Registrar un formato personalizado reutilizable
#' @param nombre Nombre identificador del formato.
#' @param fn Funcion de formato (familia `scales::*_format()`).
#' @return Invisible `nombre`.
#' @export
DefinirFormato <- function(nombre, fn) {
  if (!is.function(fn)) stop("`fn` debe ser una funcion.", call. = FALSE)
  assign(nombre, fn, envir = .formatos_racafe)
  invisible(nombre)
}

#' Obtener un formato registrado por nombre
#' @param nombre Nombre del formato.
#' @return Funcion de formato.
#' @export
ObtenerFormato <- function(nombre) {
  fn <- get0(nombre, envir = .formatos_racafe, inherits = FALSE)
  if (is.null(fn)) {
    stop(sprintf(
      "Formato '%s' no registrado. Disponibles: %s",
      nombre, paste(ls(envir = .formatos_racafe), collapse = ", ")
    ), call. = FALSE)
  }
  fn
}

#' Listar formatos registrados
#' @return Vector de caracteres con nombres disponibles.
#' @export
ListarFormatos <- function() ls(envir = .formatos_racafe)

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
