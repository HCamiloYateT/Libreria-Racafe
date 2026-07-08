% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{LimpiarNombres}
\alias{LimpiarNombres}
\title{Normalizar nombres: espacios multiples y mayusculas}
\usage{
LimpiarNombres(...)
}
\arguments{
  \item{s}{Vector de caracteres.}
}
\value{Vector de caracteres normalizado.}
\description{
Elimina espacios al inicio, fin y repetidos internos,
luego convierte a mayusculas.
}
\examples{
LimpiarNombres("  Camilo    Yate  ")
}
