% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ImprimirAnillo}
\alias{ImprimirAnillo}
\title{Grafico de anillo con plotly}
\usage{
ImprimirAnillo(...)
}
\arguments{
  \item{data}{`data.frame` con los datos.}
  \item{var_label}{Nombre de la variable categorica (etiquetas).}
  \item{var_medida}{Nombre de la variable numerica. `NULL` cuenta registros.}
  \item{funcion}{Funcion de agregacion: `"sum"` o `"n"`.}
  \item{colores}{Vector de colores. `NULL` usa la paleta corporativa.}
}
\value{Objeto plotly.}
\description{
Agrega los datos segun la variable de medida y genera un donut chart.
}
\examples{
df <- data.frame(
categoria = c("A", "A", "B", "C", "C", "C"),
valor     = c(10, 20, 30, 5, 15, 25)
)
ImprimirAnillo(df, var_label = "categoria", var_medida = "valor")
}
