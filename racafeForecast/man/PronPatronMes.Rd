% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{PronPatronMes}
\alias{PronPatronMes}
\title{Analizar patron mensual de comportamiento}
\usage{
PronPatronMes(...)
}
\arguments{
  \item{seleccion}{Objeto `pronostico_racafe` post-seleccion.}
  \item{columna}{Columna. `NULL` combina todas.}
}
\value{`tibble` con patron mensual (indice promedio por mes).}
\description{
Calcula el indice estacional promedio por mes.
}
