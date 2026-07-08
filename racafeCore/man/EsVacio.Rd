% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{EsVacio}
\alias{EsVacio}
\title{Verificar si un valor es vacio (NULL, NA o cadena vacia)}
\usage{
EsVacio(...)
}
\arguments{
  \item{x}{Valor o vector a verificar.}
}
\value{Vector logico.}
\description{
Vectorizado: aplica la verificacion a cada elemento.
}
\examples{
EsVacio("")
EsVacio(NA)
EsVacio(NULL)
EsVacio(c("texto", "", NA, "otro"))
}
