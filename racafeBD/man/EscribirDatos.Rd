% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{EscribirDatos}
\alias{EscribirDatos}
\title{Sobrescribir una tabla con el contenido de un data.frame}
\usage{
EscribirDatos(...)
}
\arguments{
  \item{df}{`data.frame` con los datos a escribir.}
  \item{tabla}{Nombre de la tabla destino.}
  \item{bd}{Nombre de la base de datos (env var por defecto).}
}
\value{Invisible `TRUE` si exitoso.}
\description{
Sobrescribir una tabla con el contenido de un data.frame
}
\examples{
\dontrun{
df <- data.frame(x = 1:3, y = letters[1:3])
EscribirDatos(df, "mi_tabla")
}
}
