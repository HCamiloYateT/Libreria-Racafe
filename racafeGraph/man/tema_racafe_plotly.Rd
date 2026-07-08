% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{tema_racafe_plotly}
\alias{tema_racafe_plotly}
\title{Obtener lista de configuracion de layout corporativo para plotly}
\usage{
tema_racafe_plotly(...)
}
\arguments{
  \item{titulo}{Titulo principal del grafico. `NULL` omite titulo.}
  \item{subtitulo}{Subtitulo. `NULL` omite.}
  \item{leyenda_h}{Logico. Si `TRUE`, ubica la leyenda horizontal debajo del eje X con margen inferior ampliado para evitar solapamiento con el titulo del eje.}
}
\value{Lista con parametros de layout para plotly.}
\description{
Aplicar con `plotly::layout(p, !!!tema_racafe_plotly())`.
}
