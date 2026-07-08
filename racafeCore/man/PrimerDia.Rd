% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{PrimerDia}
\alias{PrimerDia}
\title{Retornar el primer dia de la unidad temporal de una fecha}
\usage{
PrimerDia(...)
}
\arguments{
  \item{x}{Fecha o cadena en formato ISO 8601.}
  \item{uni}{Unidad temporal: `"month"`, `"quarter"`, `"year"`, `"week"`.}
}
\value{Objeto `Date` con el primer dia de la unidad.}
\description{
Retornar el primer dia de la unidad temporal de una fecha
}
\examples{
PrimerDia("2023-10-15")
PrimerDia("2023-10-15", uni = "year")
PrimerDia("2023-10-15", uni = "quarter")
}
