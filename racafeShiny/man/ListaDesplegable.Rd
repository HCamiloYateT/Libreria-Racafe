% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ListaDesplegable}
\alias{ListaDesplegable}
\title{Lista desplegable con pickerInput estilizado}
\usage{
ListaDesplegable(...)
}
\arguments{
  \item{inputId}{ID del input.}
  \item{label}{Etiqueta. `NULL` sin etiqueta.}
  \item{choices}{Opciones disponibles.}
  \item{selected}{Opciones seleccionadas por defecto.}
  \item{multiple}{Logico. Permitir seleccion multiple.}
  \item{fem}{Logico. Usar articulos en femenino en el placeholder.}
  \item{ns}{Funcion namespace de modulo Shiny.}
}
\value{Objeto `pickerInput` de shinyWidgets.}
\description{
Lista desplegable con pickerInput estilizado
}
\examples{
\dontrun{
ListaDesplegable("region", "Region", choices = c("Norte", "Sur"))
}
}
