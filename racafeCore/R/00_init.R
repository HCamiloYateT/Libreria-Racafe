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

#' Definir formato preconfigurado
#' @param formato Nombre del formato.
#' @param ... Argumentos adicionales pasados al generador de `scales`.
#' @return Funcion de formato.
#' @export
DefinirFormato <- function(formato, ...) {
  if (missing(formato) || length(formato) == 0L || is.na(formato[1])) {
    stop("Debe especificar un formato valido.", call. = FALSE)
  }

  formato <- tolower(trimws(as.character(formato[1])))

  generadores <- list(
    coma = function(...) scales::label_number(
      accuracy = 1,
      big.mark = ",",
      ...
    ),
    numero = function(...) scales::label_number(
      accuracy = 0.01,
      big.mark = ",",
      ...
    ),
    dinero = function(...) scales::label_number(
      accuracy = 1,
      prefix = "$",
      big.mark = ",",
      ...
    ),
    dolares = function(...) scales::label_number(
      accuracy = 0.01,
      prefix = "$",
      big.mark = ",",
      ...
    ),
    miles = function(...) scales::label_number(
      accuracy = 0.01,
      scale = 0.001,
      prefix = "$",
      big.mark = ",",
      ...
    ),
    porcentaje = function(...) scales::label_number(
      accuracy = 0.01,
      scale = 100,
      suffix = "%",
      big.mark = ",",
      ...
    ),
    cientifico = function(...) scales::label_scientific(...),
    millones = function(...) scales::label_number(
      accuracy = 0.01,
      scale = 0.000001,
      prefix = "$",
      suffix = " M",
      big.mark = ",",
      ...
    ),
    entero = function(...) scales::label_number(
      accuracy = 1,
      big.mark = ",",
      ...
    ),
    tiempo = function(...) scales::label_time(...),
    kwh = function(...) scales::label_number(
      accuracy = 0.01,
      suffix = " kWh",
      big.mark = ",",
      ...
    ),
    log = function(...) scales::label_log(...)
  )

  if (!formato %in% names(generadores)) {
    stop(
      paste0(
        "Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'dolares', ",
        "'miles', 'porcentaje', 'cientifico', 'millones', 'entero', ",
        "'tiempo', 'kwh' o 'log'."
      ),
      call. = FALSE
    )
  }

  generadores[[formato]](...)
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
