% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{aplicar_imputacion}
\alias{aplicar_imputacion}
\title{Imputar valores faltantes en una serie de tiempo}
\usage{
aplicar_imputacion(...)
}
\arguments{
  \item{ts_data}{Vector numerico o serie `ts` con posibles `NA`.}
  \item{metodo_imputacion}{Metodo a usar: `"promedio"`, `"mediana"`, `"interpolacion"`, `"constante"`, `"ultimo"`.}
  \item{valor_constante}{Valor para el metodo `"constante"`.}
  \item{prob_percentil}{Percentil para el metodo `"percentil"` (0-1).}
}
\value{Vector numerico sin `NA`.}
\description{
Imputar valores faltantes en una serie de tiempo
}
\examples{
serie <- c(1, 2, NA, 4, NA, 6)
aplicar_imputacion(serie, "interpolacion")
aplicar_imputacion(serie, "promedio")
}
