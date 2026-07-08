% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{CajaValor}
\alias{CajaValor}
\title{Caja de indicador con valor formateado y boton de detalle}
\usage{
CajaValor(...)
}
\arguments{
  \item{valor}{Valor numerico a mostrar.}
  \item{formato}{Nombre del formato registrado.}
  \item{texto}{Descripcion del indicador.}
  \item{icono}{Nombre del icono FontAwesome.}
  \item{inputId}{ID del boton de detalle.}
  \item{mostrar_boton}{Logico. Mostrar boton de detalle.}
}
\value{Objeto HTML de Shiny.}
\description{
Caja de indicador con valor formateado y boton de detalle
}
\examples{
\dontrun{
CajaValor(1250000, "dinero", "Ingresos mensuales", "chart-line",
"detalle_ingresos")
}
}
