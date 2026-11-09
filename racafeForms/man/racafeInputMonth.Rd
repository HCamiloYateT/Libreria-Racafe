\name{racafeInputMonth}
\alias{racafeInputMonth}
\alias{racafeUpdateMonth}
\title{Selector corporativo de año y mes}
\usage{
racafeInputMonth(id, label = "Año y mes", value = Sys.Date(), min = NULL,
  max = NULL, ns = NULL, width = "100\%", disabled = FALSE,
  required = FALSE, help = NULL, error = NULL)
racafeUpdateMonth(session, id, value = NULL, min = NULL, max = NULL)
}
\arguments{
\item{id}{ID local del control.}
\item{label}{Etiqueta visible.}
\item{value,min,max}{Fechas normalizadas al primer día de mes.}
\item{ns}{Namespace opcional del módulo.}
\item{width}{Ancho CSS del control.}
\item{disabled,required}{Estado del control.}
\item{help,error}{Mensajes de ayuda y validación.}
\item{session}{Sesión Shiny del módulo.}
}
\description{Usa el calendario corporativo en vista mensual, mostrando mes y año y conservando un valor Date.}
\value{Control Shiny o actualización de su selección y límites.}
