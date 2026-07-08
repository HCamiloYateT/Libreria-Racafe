% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{cajaValor_server}
\alias{cajaValor_server}
\title{Server del modulo CajaValor}
\usage{
cajaValor_server(...)
}
\arguments{
  \item{id}{ID del modulo Shiny.}
  \item{valor_r}{Reactivo que retorna el valor numerico.}
  \item{formato}{Nombre del formato registrado en `DefinirFormato`.}
  \item{meta}{Valor de meta para colorear condicionalmente. `NA` desactiva.}
}
\value{Lista con `click_detalle`: reactivo del boton de detalle.}
\description{
Recibe un reactivo con el valor y aplica formato. Exporta el
evento del boton de detalle para el modulo padre.
}
\examples{
\dontrun{
# En UI del modulo padre:
cajaValor_ui("ingresos", icono = "dollar-sign", texto = "Ingresos")

# En Server del modulo padre:
modulo <- cajaValor_server("ingresos",
valor_r = reactive(sum(datos()$ventas)),
formato = "dinero"
)
observeEvent(modulo$click_detalle(), { ... })
}
}
