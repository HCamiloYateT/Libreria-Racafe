% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{BotonDescarga}
\alias{BotonDescarga}
\title{Botón de descarga estilizado con clases racafe y tamaños unificados}
\usage{
BotonDescarga(...)
}
\arguments{
  \item{button_id}{String. `outputId` del botón.}
  \item{icono}{String. Nombre FontAwesome. Default `"download"`.}
  \item{color_fondo}{String. Color de fondo reconocido por R (nombre o hexadecimal). Por defecto usa el mismo color base de `racafeShiny::Boton()`.}
  \item{color_fuente}{String. Color de fuente reconocido por R (nombre o hexadecimal). Por defecto usa el mismo color de texto de `racafeShiny::Boton()`.}
  \item{color_hover}{Color de fondo al hover. `NULL` aplica oscurecimiento automatico.}
  \item{ns}{`NULL` o función de namespace (ej. `shiny::NS`).}
  \item{title}{String o `NULL`. Tooltip del contenedor.}
  \item{size}{String. Tamaño: `"xxs"`, `"xs"`, `"sm"`, `"md"`, `"lg"`, `"xl"`, `"xxl"`.}
  \item{align}{String. Alineación: `"left"`, `"center"` o `"right"`. }
}
\value{`shiny.tag` div contenedor con el `downloadButton` estilizado.}
\description{
Genera un `downloadButton` con clase base `racafe-btn-descarga`, tamaños
normalizados y color configurable vía atributo `data-racafe-color`.
}
