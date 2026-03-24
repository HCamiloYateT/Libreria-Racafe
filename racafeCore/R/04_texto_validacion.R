# ============================================================
# Seccion 04 — Texto y validacion
# Limpieza, normalizacion, validadores y helpers de cadenas
# ============================================================


# ---- Limpieza de texto ----

#' Normalizar nombres: espacios multiples y mayusculas
#'
#' Elimina espacios al inicio, fin y repetidos internos,
#' luego convierte a mayusculas.
#'
#' @param s Vector de caracteres.
#' @return Vector de caracteres normalizado.
#' @export
#' @examples
#' LimpiarNombres("  Camilo    Yate  ")
LimpiarNombres <- function(s) {
  s <- trimws(s)
  s <- gsub("\\s+", " ", s)
  toupper(s)
}


#' Limpiar caracteres no deseados de un texto
#'
#' Aplica en un unico pass las transformaciones seleccionadas
#' usando stringi para soporte Unicode completo.
#'
#' @param x Vector de caracteres.
#' @param rem_espacios Logico. Eliminar todos los espacios.
#' @param rem_numeros Logico. Eliminar digitos.
#' @param rem_caresp Logico. Eliminar caracteres especiales (no alfanumericos).
#' @param rem_acentos Logico. Remover acentos y diacriticos.
#' @return Vector de caracteres limpio.
#' @export
#' @examples
#' LimpiarCadena("!Hola, mundo 123!")
#' LimpiarCadena("  texto  con  espacios  ", rem_espacios = TRUE)
LimpiarCadena <- function(
    x,
    rem_espacios = FALSE,
    rem_numeros  = TRUE,
    rem_caresp   = TRUE,
    rem_acentos  = TRUE) {

  if (rem_acentos) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }

  patron <- character(0)
  if (rem_numeros)  patron <- c(patron, "\\d")
  if (rem_caresp)   patron <- c(patron, "[^a-zA-Z0-9\\s]")
  if (rem_espacios) patron <- c(patron, "\\s")

  if (length(patron) > 0) {
    patron_unido <- paste(patron, collapse = "|")
    x <- gsub(patron_unido, "", x, perl = TRUE)
  }

  trimws(gsub("\\s+", " ", x))
}


#' Unir cadenas omitiendo NAs
#'
#' @param ... Vectores de caracteres a unir.
#' @param sep Separador entre elementos.
#' @param collapse Separador para colapsar el resultado final. `NULL` no colapsa.
#' @param na.rm Logico. Si `TRUE`, ignora valores `NA`.
#' @return Cadena unida.
#' @export
#' @examples
#' UnirCadenas("Hola", NA, "Mundo", sep = "-", na.rm = TRUE)
#' UnirCadenas("A", "B", "C", sep = "_")
UnirCadenas <- function(..., sep = " ", collapse = NULL, na.rm = FALSE) {
  partes <- list(...)

  if (na.rm) {
    partes <- lapply(partes, function(x) {
      x[is.na(x)] <- ""
      x
    })
  }

  resultado <- do.call(paste, c(partes, sep = sep))

  if (!is.null(collapse)) {
    resultado <- paste(resultado, collapse = collapse)
  }

  resultado
}


#' Valores unicos ordenados de un vector
#'
#' @param x Vector de cualquier tipo.
#' @return Vector con valores unicos en orden ascendente.
#' @export
#' @examples
#' Unicos(c("b", "a", "a", "c"))
Unicos <- function(x) {
  sort(unique(x))
}


# ---- Validadores ----

#' Verificar si un valor es vacio (NULL, NA o cadena vacia)
#'
#' Vectorizado: aplica la verificacion a cada elemento.
#'
#' @param x Valor o vector a verificar.
#' @return Vector logico.
#' @export
#' @examples
#' EsVacio("")
#' EsVacio(NA)
#' EsVacio(NULL)
#' EsVacio(c("texto", "", NA, "otro"))
EsVacio <- function(x) {
  if (is.null(x)) return(TRUE)
  is.na(x) | (is.character(x) & nchar(trimws(x)) == 0)
}


#' Verificar si una cadena representa un entero positivo
#'
#' @param s Cadena de caracteres.
#' @return Logico. `TRUE` si la cadena es un entero positivo (>0).
#' @export
#' @examples
#' EsEnteroPositivo("123")
#' EsEnteroPositivo("0")
#' EsEnteroPositivo("-5")
#' EsEnteroPositivo("abc")
EsEnteroPositivo <- function(s) {
  if (EsVacio(s)) return(FALSE)
  grepl("^[1-9]\\d*$", trimws(s))
}


#' Verificar si una cadena representa un numero positivo
#'
#' Los valores `NA` y cadenas vacias retornan `FALSE`.
#'
#' @param cadena Cadena de caracteres.
#' @return Logico.
#' @export
#' @examples
#' EsNumero("12.3")
#' EsNumero("")
#' EsNumero(NA)
#' EsNumero("-5.2")
EsNumero <- function(cadena) {
  if (is.na(cadena) || nchar(trimws(cadena)) == 0) return(FALSE)
  grepl("^\\d+(\\.\\d+)?$", trimws(cadena))
}


#' Validar formato de numero telefonico colombiano
#'
#' Acepta numeros celulares de 10 digitos que inicien con 3.
#'
#' @param tel Cadena con el numero telefonico.
#' @return Logico.
#' @export
#' @examples
#' EsNumTelefono("3123456789")
#' EsNumTelefono("1234567890")
EsNumTelefono <- function(tel) {
  if (EsVacio(tel)) return(FALSE)
  tel_limpio <- gsub("[^0-9]", "", tel)
  grepl("^3[0-9]{9}$", tel_limpio)
}


#' Validar formato de correo electronico
#'
#' @param email Cadena con la direccion de correo.
#' @return Logico.
#' @export
#' @examples
#' EsEmail("usuario@racafe.com")
#' EsEmail("invalido@")
#' EsEmail("sin_arroba.com")
EsEmail <- function(email) {
  if (EsVacio(email)) return(FALSE)
  grepl(
    "^[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}$",
    trimws(email),
    perl = TRUE
  )
}
