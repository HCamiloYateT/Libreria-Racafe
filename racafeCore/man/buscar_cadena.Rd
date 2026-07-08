% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{buscar_cadena}
\alias{buscar_cadena}
\title{Buscar una cadena dentro de archivos en una ruta}
\usage{
buscar_cadena(...)
}
\arguments{
  \item{cadena}{Cadena a buscar.}
  \item{ruta}{Ruta base donde se realizara la busqueda.}
  \item{patron_archivo}{Patron de archivos a incluir (por defecto `"*.R"`).}
}
\value{Vector de caracteres con resultados de `grep`.}
\description{
Ejecuta `grep` de forma recursiva y devuelve coincidencias con numero de linea.
}
\examples{
\dontrun{
buscar_cadena("TopAbsoluto", ".", "*.R")
}
}
