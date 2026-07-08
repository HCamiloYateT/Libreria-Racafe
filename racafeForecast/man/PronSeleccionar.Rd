% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{PronSeleccionar}
\alias{PronSeleccionar}
\title{Seleccionar el mejor modelo por columna}
\usage{
PronSeleccionar(...)
}
\arguments{
  \item{resultado_pronostico}{Objeto `pronostico_racafe`.}
  \item{columna}{Columna a evaluar. `NULL` evalua todas.}
  \item{...}{Reservado para extensiones (criterio de seleccion).}
}
\value{Objeto `pronostico_racafe` filtrado al mejor modelo por columna.}
\description{
Elige el modelo con menor RMSE que no haya fallado.
}
