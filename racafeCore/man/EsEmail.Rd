% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{EsEmail}
\alias{EsEmail}
\title{Validar formato de correo electronico}
\usage{
EsEmail(...)
}
\arguments{
  \item{email}{Cadena con la direccion de correo.}
}
\value{Logico.}
\description{
Validar formato de correo electronico
}
\examples{
EsEmail("usuario@racafe.com")
EsEmail("invalido@")
EsEmail("sin_arroba.com")
}
