% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{gt_mensaje_vacio}
\alias{gt_mensaje_vacio}
\title{Generar tabla gt de mensaje vacio}
\usage{
gt_mensaje_vacio(...)
}
\arguments{
  \item{mensaje}{Texto a mostrar.}
}
\value{Objeto `gt_tbl` minimalista con el mensaje.}
\description{
Util para mostrar en Shiny cuando no hay datos disponibles.
}
\examples{
gt_mensaje_vacio()
gt_mensaje_vacio("Sin resultados para los filtros seleccionados")
}
