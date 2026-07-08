% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{LimpiarCadena}
\alias{LimpiarCadena}
\title{Limpiar caracteres no deseados de un texto}
\usage{
LimpiarCadena(...)
}
\arguments{
  \item{x}{Vector de caracteres.}
  \item{rem_espacios}{Logico. Eliminar todos los espacios.}
  \item{rem_numeros}{Logico. Eliminar digitos.}
  \item{rem_caresp}{Logico. Eliminar caracteres especiales (no alfanumericos).}
  \item{rem_acentos}{Logico. Remover acentos y diacriticos.}
}
\value{Vector de caracteres limpio.}
\description{
Aplica en un unico pass las transformaciones seleccionadas
usando stringi para soporte Unicode completo.
}
\examples{
LimpiarCadena("!Hola, mundo 123!")
LimpiarCadena("  texto  con  espacios  ", rem_espacios = TRUE)
}
