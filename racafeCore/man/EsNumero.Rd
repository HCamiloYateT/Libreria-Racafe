% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{EsNumero}
\alias{EsNumero}
\title{Verificar si una cadena representa un numero positivo}
\usage{
EsNumero(...)
}
\arguments{
  \item{cadena}{Cadena de caracteres.}
}
\value{Logico.}
\description{
Los valores `NA` y cadenas vacias retornan `FALSE`.
}
\examples{
EsNumero("12.3")
EsNumero("")
EsNumero(NA)
EsNumero("-5.2")
}
