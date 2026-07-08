% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{CargarDatos}
\alias{CargarDatos}
\title{Recuperar datos de una tabla con filtro opcional}
\usage{
CargarDatos(...)
}
\arguments{
  \item{tabla}{Nombre de la tabla.}
  \item{condicion}{Cadena SQL para clausula WHERE (sin la palabra WHERE).}
}
\value{`data.frame` con los resultados.}
\description{
Recuperar datos de una tabla con filtro opcional
}
\examples{
\dontrun{
df <- CargarDatos("mi_tabla")
df_filtrado <- CargarDatos("mi_tabla", "x > 10")
}
}
