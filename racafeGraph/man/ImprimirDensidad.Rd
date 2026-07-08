% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ImprimirDensidad}
\alias{ImprimirDensidad}
\title{Histograma con densidad kernel en escala logaritmica}
\usage{
ImprimirDensidad(...)
}
\arguments{
  \item{datos}{`data.frame` con los datos.}
  \item{columna}{Nombre de la columna numerica a graficar.}
  \item{titulo}{Titulo del grafico.}
  \item{formato}{Argumento reservado para compatibilidad (sin uso actual).}
}
\value{Objeto plotly.}
\description{
Combina histograma (porcentaje) y densidad kernel para distribuciones
sesgadas. Util para explorar variables con colas largas (ingresos, volumen).
}
\examples{
set.seed(123)
ventas <- data.frame(ingresos = rgamma(250, shape = 3, rate = 0.7))
ImprimirDensidad(ventas, "ingresos", "Ingresos diarios", formato = "numero")
}
