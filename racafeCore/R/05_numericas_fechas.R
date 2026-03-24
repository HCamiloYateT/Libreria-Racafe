# ============================================================
# Seccion 05 — Funciones numericas y de fechas
# Calculo, redondeo, variacion, moda y manipulacion de fechas
# ============================================================


# ---- Funciones numericas ----

#' Reemplazar errores de evaluacion por cero
#'
#' @param x Expresion o valor. Si genera error retorna 0.
#' @return Valor numerico o 0 en caso de error.
#' @export
#' @examples
#' SiError_0(log(1))
#' SiError_0(tryCatch(log(-1), warning = function(w) w))
SiError_0 <- function(x) {
  tryCatch(
    {
      val <- x
      if (inherits(val, "error") || inherits(val, "warning") ||
          is.nan(val) || is.infinite(val)) {
        0
      } else {
        val
      }
    },
    error   = function(e) 0,
    warning = function(w) 0
  )
}


#' Calcular variacion porcentual entre dos valores
#'
#' @param ini Valor inicial.
#' @param fin Valor final.
#' @return Variacion porcentual como decimal (ej. 0.20 = 20%).
#'   Retorna `NA` si `ini` es cero o `NA`.
#' @export
#' @examples
#' Variacion(100, 120)
#' Variacion(0, 50)
#' Variacion(NA, 50)
Variacion <- function(ini, fin) {
  dplyr::case_when(
    is.na(ini) | is.na(fin) ~ NA_real_,
    ini == 0                ~ NA_real_,
    TRUE                    ~ (fin - ini) / abs(ini)
  )
}


#' Calcular la moda de un vector
#'
#' Para vectores con empate devuelve el primer valor en orden de aparicion.
#'
#' @param x Vector de cualquier tipo atomico.
#' @param na.rm Logico. Si `TRUE`, excluye `NA` del calculo.
#' @return Escalar con el valor mas frecuente.
#' @export
#' @examples
#' Moda(c(1, 2, 2, 3))
#' Moda(c("a", "b", "b", "c"))
Moda <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' Redondear al multiplo mas cercano
#'
#' @param x Numero o vector numerico.
#' @param multiple Multiplo objetivo.
#' @return Numero redondeado al multiplo mas cercano.
#' @export
#' @examples
#' RedondearMultiplo(17, 5)
#' RedondearMultiplo(c(13, 17, 22), 5)
RedondearMultiplo <- function(x, multiple) {
  stopifnot(is.numeric(x), is.numeric(multiple), multiple != 0)
  round(x / multiple) * multiple
}


# ---- Funciones de fechas ----

#' Retornar el primer dia de la unidad temporal de una fecha
#'
#' @param x Fecha o cadena en formato ISO 8601.
#' @param uni Unidad temporal: `"month"`, `"quarter"`, `"year"`, `"week"`.
#' @return Objeto `Date` con el primer dia de la unidad.
#' @export
#' @examples
#' PrimerDia("2023-10-15")
#' PrimerDia("2023-10-15", uni = "year")
#' PrimerDia("2023-10-15", uni = "quarter")
PrimerDia <- function(x, uni = "month") {
  fecha <- as.Date(x)
  uni <- match.arg(uni, c("month", "quarter", "year", "week"))
  lubridate::floor_date(fecha, unit = uni)
}


#' Convertir fecha a texto con formato personalizado
#'
#' @param x Objeto `Date` o cadena ISO 8601.
#' @param formato Cadena de formato para `format()`.
#'   Por defecto: `"%B de %Y"` (ej. "octubre de 2023").
#' @param ... Argumentos adicionales para `format()`.
#' @return Cadena de texto con la fecha formateada.
#' @export
#' @examples
#' FechaTexto(as.Date("2023-10-15"))
#' FechaTexto(as.Date("2023-10-15"), formato = "%d/%m/%Y")
FechaTexto <- function(x, formato = "%B de %Y", ...) {
  format(as.Date(x), format = formato, ...)
}


#' Calcular edad cumplida en anios entre dos fechas
#'
#' @param from Fecha de nacimiento o fecha inicial.
#' @param to Fecha de referencia para el calculo.
#' @return Entero con los anios cumplidos.
#' @export
#' @examples
#' EdadCumplida(as.Date("1990-05-25"), Sys.Date())
#' EdadCumplida(as.Date("2000-01-01"), as.Date("2023-12-31"))
EdadCumplida <- function(from, to) {
  from <- as.Date(from)
  to   <- as.Date(to)
  as.integer(lubridate::interval(from, to) / lubridate::years(1))
}
