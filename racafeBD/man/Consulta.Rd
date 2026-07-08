% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{Consulta}
\alias{Consulta}
\title{Ejecutar una consulta SQL arbitraria}
\usage{
Consulta(...)
}
\arguments{
  \item{consulta}{Cadena SQL completa.}
  \item{bd}{Nombre de la base de datos.}
}
\value{`data.frame` con los resultados, columnas limpias con janitor.}
\description{
Ejecutar una consulta SQL arbitraria
}
\examples{
\dontrun{
resultado <- Consulta("SELECT COUNT(*) AS n FROM mi_tabla")
}
}
