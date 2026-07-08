% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{Moda}
\alias{Moda}
\title{Calcular la moda de un vector}
\usage{
Moda(...)
}
\arguments{
  \item{x}{Vector de cualquier tipo atomico.}
  \item{na.rm}{Logico. Si `TRUE`, excluye `NA` del calculo.}
}
\value{Escalar con el valor mas frecuente.}
\description{
Para vectores con empate devuelve el primer valor en orden de aparicion.
}
\examples{
Moda(c(1, 2, 2, 3))
Moda(c("a", "b", "b", "c"))
}
