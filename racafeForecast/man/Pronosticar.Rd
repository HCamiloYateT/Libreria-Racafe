% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{Pronosticar}
\alias{Pronosticar}
\title{Orquestar el flujo completo de pronosticos}
\usage{
Pronosticar(...)
}
\arguments{
  \item{df}{`data.frame` con la serie de tiempo.}
  \item{fecha_col}{Nombre de la columna de fechas.}
  \item{valor_cols}{Vector de nombres de columnas a pronosticar. `NULL` selecciona todas las columnas numericas excepto la fecha.}
  \item{nivel_confianza}{Nivel de confianza para intervalos (0-1).}
  \item{prop_train}{Proporcion de datos para entrenamiento (0-1).}
  \item{h_periods}{Horizonte de pronostico en periodos.}
  \item{metodo_imputacion}{Metodo de imputacion de NA.}
  \item{...}{Argumentos adicionales para `ejecutar_pronosticos`.}
}
\value{Objeto `pronostico_racafe`.}
\description{
Divide la serie en train/test, imputa NA, ajusta multiples modelos
y retorna un objeto `pronostico_racafe` estandarizado.
}
\examples{
\dontrun{
df <- data.frame(
fecha  = seq(as.Date("2022-01-01"), by = "month", length.out = 36),
ventas = cumsum(rnorm(36, mean = 100, sd = 15))
)
pron <- Pronosticar(df, fecha_col = "fecha", valor_cols = "ventas")
}
}
