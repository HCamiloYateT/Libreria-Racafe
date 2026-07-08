% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{cajaValor_ui}
\alias{cajaValor_ui}
\title{UI del modulo CajaValor}
\usage{
cajaValor_ui(...)
}
\arguments{
  \item{id}{ID del modulo Shiny.}
  \item{icono}{Nombre del icono FontAwesome.}
  \item{texto}{Etiqueta descriptiva del indicador.}
  \item{mostrar_boton}{Logico. Mostrar boton de detalle.}
}
\value{`tagList` con la UI del modulo.}
\description{
Genera la estructura HTML del indicador. Toda la logica numerica
vive en el servidor del modulo (`cajaValor_server`).
}
