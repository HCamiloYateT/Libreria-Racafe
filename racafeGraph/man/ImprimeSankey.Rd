% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ImprimeSankey}
\alias{ImprimeSankey}
\title{Diagrama Sankey con plotly}
\usage{
ImprimeSankey(...)
}
\arguments{
  \item{data}{`data.frame` con los datos de flujo.}
  \item{vars}{Vector de nombres de columnas que definen los nodos (origen -> destino -> ...).}
  \item{fun}{Funcion de agregacion: `"sum"` o `"n"`.}
  \item{var}{Nombre de la variable numerica para `fun = "sum"`. `NULL` cuenta.}
  \item{colores}{Vector de colores para los nodos.}
}
\value{Objeto plotly con diagrama Sankey.}
\description{
Diagrama Sankey con plotly
}
\examples{
\dontrun{
df <- data.frame(origen = c("A","A","B"), destino = c("X","Y","X"),
valor = c(10, 20, 15))
ImprimeSankey(df, vars = c("origen","destino"), fun = "sum", var = "valor")
}
}
