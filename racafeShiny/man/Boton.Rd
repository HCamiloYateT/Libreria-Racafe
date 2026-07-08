% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{Boton}
\alias{Boton}
\title{Boton de accion generico con alineacion configurable}
\usage{
Boton(...)
}
\arguments{
  \item{id}{ID del boton.}
  \item{label}{Texto del boton. Use `NULL` para omitir texto.}
  \item{icono}{Nombre del icono Font Awesome. Use `NULL` para omitir icono.}
  \item{align}{Alineacion: `"left"`, `"center"`, `"right"`.}
  \item{size}{Tamano visual del boton: `"xxs"`, `"xs"`, `"sm"`, `"md"`, `"lg"`, `"xl"`, `"xxl"`.}
  \item{color_fondo}{Color de fondo base (nombre R o hex CSS).}
  \item{color_fuente}{Color de texto/icono.}
  \item{color_hover}{Color de fondo al hover. `NULL` aplica oscurecimiento automatico.}
  \item{label_posicion}{Posicion del label respecto al icono: `"right"` o `"below"`.}
  \item{titulo}{Tooltip del boton al pasar el cursor. `NULL` para omitir.}
  \item{...}{Argumentos adicionales para `shiny::actionButton`.}
}
\value{Componente Shiny.}
\description{
Boton de accion generico con alineacion configurable
}
\examples{
\dontrun{
Boton("guardar_form", label = "Guardar cambios", align = "center")
Boton("guardar_icono", label = NULL, icono = "floppy-disk")
Boton("guardar_completo", label = "Guardar", icono = "floppy-disk")
Boton("guardar_abajo", label = "Guardar", icono = "floppy-disk", label_posicion = "below")
}
}
