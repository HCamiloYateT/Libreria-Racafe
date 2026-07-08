% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{PronMetricas}
\alias{PronMetricas}
\title{Calcular metricas de precision por columna}
\usage{
PronMetricas(...)
}
\arguments{
  \item{resultado_pronostico}{Objeto `pronostico_racafe`.}
  \item{columna}{Columna a evaluar. `NULL` evalua todas.}
}
\value{`tibble` con RMSE, MAE y MAPE por modelo y columna.}
\description{
Calcular metricas de precision por columna
}
