% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ejecutar_pronosticos}
\alias{ejecutar_pronosticos}
\title{Ajustar multiples modelos y comparar resultados}
\usage{
ejecutar_pronosticos(...)
}
\arguments{
  \item{train_data}{Vector numerico de entrenamiento.}
  \item{test_data}{Vector numerico de prueba.}
  \item{h_periods}{Horizonte de pronostico.}
  \item{fechas_futuras}{Vector de fechas para el horizonte.}
  \item{nivel_confianza}{Nivel de confianza para intervalos (0-1).}
  \item{...}{Argumentos adicionales (reservado para extensiones).}
}
\value{Lista con resultados por modelo.}
\description{
Ajusta ETS, ARIMA, NNETAR, TBATS y modelos de tendencia sobre los datos
de entrenamiento y evalua sobre los datos de prueba.
}
