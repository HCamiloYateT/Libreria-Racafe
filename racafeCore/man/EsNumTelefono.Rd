% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{EsNumTelefono}
\alias{EsNumTelefono}
\title{Validar formato de numero telefonico colombiano}
\usage{
EsNumTelefono(...)
}
\arguments{
  \item{tel}{Cadena con el numero telefonico.}
}
\value{Logico.}
\description{
Acepta numeros celulares de 10 digitos que inicien con 3.
}
\examples{
EsNumTelefono("3123456789")
EsNumTelefono("1234567890")
}
