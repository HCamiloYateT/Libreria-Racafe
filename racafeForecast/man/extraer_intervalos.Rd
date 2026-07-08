% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{extraer_intervalos}
\alias{extraer_intervalos}
\title{Extraer intervalos de confianza de un objeto de pronostico}
\usage{
extraer_intervalos(...)
}
\arguments{
  \item{forecast_obj}{Objeto de pronostico generado por el paquete `forecast`.}
  \item{nivel_conf}{Nivel de confianza: `0.80` o `0.95`.}
}
\value{`tibble` con columnas `lower` y `upper`.}
\description{
Extraer intervalos de confianza de un objeto de pronostico
}
