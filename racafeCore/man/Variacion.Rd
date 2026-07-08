% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{Variacion}
\alias{Variacion}
\title{Calcular variacion porcentual entre dos valores}
\usage{
Variacion(...)
}
\arguments{
  \item{ini}{Valor inicial.}
  \item{fin}{Valor final.}
}
\value{Variacion porcentual como decimal (ej. 0.20 = 20\%). Retorna `NA` si `ini` es cero o `NA`.}
\description{
Calcular variacion porcentual entre dos valores
}
\examples{
Variacion(100, 120)
Variacion(0, 50)
Variacion(NA, 50)
}
