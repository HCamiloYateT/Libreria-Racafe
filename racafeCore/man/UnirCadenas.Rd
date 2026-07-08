% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{UnirCadenas}
\alias{UnirCadenas}
\title{Unir cadenas omitiendo NAs}
\usage{
UnirCadenas(...)
}
\arguments{
  \item{...}{Vectores de caracteres a unir.}
  \item{sep}{Separador entre elementos.}
  \item{collapse}{Separador para colapsar el resultado final. `NULL` no colapsa.}
  \item{na.rm}{Logico. Si `TRUE`, ignora valores `NA`.}
}
\value{Cadena unida.}
\description{
Unir cadenas omitiendo NAs
}
\examples{
UnirCadenas("Hola", NA, "Mundo", sep = "-", na.rm = TRUE)
UnirCadenas("A", "B", "C", sep = "_")
}
