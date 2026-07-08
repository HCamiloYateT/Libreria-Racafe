% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{InputNumerico}
\alias{InputNumerico}
\title{Input numerico personalizado con layout de columnas}
\usage{
InputNumerico(...)
}
\arguments{
  \item{id}{ID del input Shiny.}
  \item{label}{Etiqueta visible del input.}
  \item{value}{Valor inicial.}
  \item{dec}{Numero de decimales permitidos.}
  \item{max}{Valor maximo permitido. `NULL` sin limite.}
  \item{min}{Valor minimo permitido. `NULL` sin limite.}
  \item{type}{Nombre del formato registrado para mostrar (solo visual).}
  \item{label_col}{Ancho de la columna del label (sobre 12).}
  \item{input_col}{Ancho de la columna del input (sobre 12).}
  \item{width}{Ancho CSS del input.}
  \item{ns}{Funcion namespace de modulo Shiny. `NULL` si no es modulo.}
}
\value{`tagList` de Shiny con el input numerico.}
\description{
Input numerico personalizado con layout de columnas
}
\examples{
\dontrun{
InputNumerico("ventas", "Ventas", 1000, dec = 0)
}
}
