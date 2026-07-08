% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ReemplazarDatos}
\alias{ReemplazarDatos}
\title{Reemplazar registros en una tabla segun llaves}
\usage{
ReemplazarDatos(...)
}
\arguments{
  \item{df}{`data.frame` con los nuevos datos.}
  \item{tabla}{Nombre de la tabla.}
  \item{llaves}{Lista nombrada con pares columna = valor para el DELETE.}
  \item{bd}{Nombre de la base de datos.}
}
\value{Invisible `TRUE` si exitoso.}
\description{
Elimina filas que coincidan con las llaves especificadas e inserta
los nuevos datos. Operacion atomica via transaccion.
}
\examples{
\dontrun{
df <- data.frame(id = 1, fecha = as.Date("2024-01-01"), valor = 100)
ReemplazarDatos(df, "mi_tabla", list(id = 1, fecha = "2024-01-01"))
}
}
