% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{EdadCumplida}
\alias{EdadCumplida}
\title{Calcular edad cumplida en anios entre dos fechas}
\usage{
EdadCumplida(...)
}
\arguments{
  \item{from}{Fecha de nacimiento o fecha inicial.}
  \item{to}{Fecha de referencia para el calculo.}
}
\value{Entero con los anios cumplidos.}
\description{
Calcular edad cumplida en anios entre dos fechas
}
\examples{
EdadCumplida(as.Date("1990-05-25"), Sys.Date())
EdadCumplida(as.Date("2000-01-01"), as.Date("2023-12-31"))
}
