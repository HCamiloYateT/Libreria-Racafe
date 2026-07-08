% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{FormatearTexto}
\alias{FormatearTexto}
\title{Aplicar estilo HTML a texto plano}
\usage{
FormatearTexto(...)
}
\arguments{
  \item{x}{Texto a estilizar.}
  \item{negrita}{Logico. Aplicar negrita.}
  \item{color}{Color del texto.}
  \item{tamano_pct}{Multiplicador del tamano de fuente relativo (1 = 100\%).}
  \item{alineacion}{Alineacion del texto: `"left"`, `"center"`, `"right"`.}
  \item{transform}{Transformacion CSS: `"none"`, `"uppercase"`, `"lowercase"`.}
}
\value{Cadena HTML con el texto estilizado.}
\description{
Aplicar estilo HTML a texto plano
}
\examples{
FormatearTexto("Meta alcanzada", color = "#28B78D")
FormatearTexto("URGENTE", negrita = TRUE, transform = "uppercase")
}
