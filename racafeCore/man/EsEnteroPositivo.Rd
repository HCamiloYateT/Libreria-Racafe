% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{EsEnteroPositivo}
\alias{EsEnteroPositivo}
\title{Verificar si una cadena representa un entero positivo}
\usage{
EsEnteroPositivo(...)
}
\arguments{
  \item{s}{Cadena de caracteres.}
}
\value{Logico. `TRUE` si la cadena es un entero positivo (>0).}
\description{
Verificar si una cadena representa un entero positivo
}
\examples{
EsEnteroPositivo("123")
EsEnteroPositivo("0")
EsEnteroPositivo("-5")
EsEnteroPositivo("abc")
}
