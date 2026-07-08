% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{FormatrearTexto}
\alias{FormatrearTexto}
\title{Alias de compatibilidad para FormatearTexto}
\usage{
FormatrearTexto(...)
}
\arguments{
  \item{x}{Texto o vector de textos a formatear.}
  \item{color}{Color CSS del texto.}
  \item{negrita}{Lógico. Si `TRUE`, aplica negrita.}
  \item{tamano}{Tamaño CSS opcional.}
  \item{alineacion}{Alineación CSS opcional.}
  \item{transform}{Transformación de texto: `"none"`, `"uppercase"`, `"lowercase"` o `"capitalize"`.}
}
\value{Texto HTML formateado mediante `FormatearTexto()`.}
\description{
Alias mantenido por compatibilidad para código existente que usa el nombre `FormatrearTexto`.
}
\examples{
FormatrearTexto("Meta alcanzada", color = "#28B78D", negrita = TRUE)
}
