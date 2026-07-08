% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{RevisarDuplicados}
\alias{RevisarDuplicados}
\title{Revisar llaves duplicadas entre dos tablas}
\usage{
RevisarDuplicados(...)
}
\arguments{
  \item{x}{Primera tabla.}
  \item{y}{Segunda tabla.}
  \item{by}{Columnas de llave para la comparacion.}
}
\value{Lista con dos elementos: `duplicados_x` y `duplicados_y`.}
\description{
Revisar llaves duplicadas entre dos tablas
}
\examples{
a <- data.frame(id = c(1, 1, 2))
b <- data.frame(id = c(1, 2, 2))
RevisarDuplicados(a, b, by = "id")
}
