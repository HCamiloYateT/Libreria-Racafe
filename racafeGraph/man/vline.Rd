% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{vline}
\alias{vline}
\title{Crear linea vertical de referencia para graficos plotly}
\usage{
vline(...)
}
\arguments{
  \item{x}{Posicion en el eje X.}
  \item{color}{Color de la linea.}
}
\value{Lista con especificacion de shape para `plotly::layout(shapes = ...)`.}
\description{
Crear linea vertical de referencia para graficos plotly
}
\examples{
\dontrun{
plotly::plot_ly() |>
plotly::layout(shapes = list(vline(10, "#28B78D")))
}
}
