% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{InputFecha}
\alias{InputFecha}
\title{Selector de fecha con configuracion derivada por tipo de vista}
\usage{
InputFecha(...)
}
\arguments{
  \item{id}{String. `inputId` del widget.}
  \item{label}{String o `NULL`. Etiqueta visible. `NULL` la omite.}
  \item{value}{Date. Fecha inicial seleccionada. Default `Sys.Date()`.}
  \item{tipo}{String. Granularidad: `"dia"`, `"mes"`, `"anio"`.}
  \item{min_date}{Date o `NULL`. Fecha minima seleccionable. `NULL` sin limite.}
  \item{max_date}{Date o `NULL`. Fecha maxima seleccionable. `NULL` sin limite.}
  \item{inline}{Logical. `TRUE` muestra el calendario embebido (sin dropdown). Default `FALSE`.}
  \item{width}{String. Ancho CSS del input. Default `"100\%"`.}
  \item{...}{Argumentos adicionales pasados a `airDatepickerInput()`. }
}
\value{`shiny.tag` del widget `airDatepickerInput` configurado.}
\description{
Wrapper de [shinyWidgets::airDatepickerInput()] que deriva automaticamente
`view`, `minView`, `dateFormat`, `startView` y constantes de idioma a
partir del tipo de granularidad solicitada. Solo requiere los parametros
sustantivos: id, valor inicial, tipo y rango.
}
\examples{
InputFecha("fecha_dia",  label = "Dia",  tipo = "dia")
InputFecha("fecha_mes",  label = "Mes",  tipo = "mes",  value = Sys.Date(),
min_date = as.Date("2023-01-01"), max_date = Sys.Date())
InputFecha("fecha_anio", label = "Anio", tipo = "anio", inline = TRUE)
}
