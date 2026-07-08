% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{hline}
\alias{hline}
\title{Crear linea horizontal de referencia para graficos plotly}
\usage{
hline(...)
}
\arguments{
  \item{y}{Posicion en el eje Y.}
  \item{color}{Color de la linea.}
}
\value{Lista con especificacion de shape para `plotly::layout(shapes = ...)`.}
\description{
Crear linea horizontal de referencia para graficos plotly
}
\examples{
\dontrun{
plotly::plot_ly() |>
plotly::layout(shapes = list(hline(0.5)))
}
}
