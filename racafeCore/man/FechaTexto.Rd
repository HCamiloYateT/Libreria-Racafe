% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{FechaTexto}
\alias{FechaTexto}
\title{Convertir fecha a texto con formato personalizado}
\usage{
FechaTexto(...)
}
\arguments{
  \item{x}{Objeto `Date` o cadena ISO 8601.}
  \item{formato}{Cadena de formato para `format()`. Por defecto: `"\%B de \%Y"` (ej. "octubre de 2023").}
  \item{...}{Argumentos adicionales para `format()`.}
}
\value{Cadena de texto con la fecha formateada.}
\description{
Convertir fecha a texto con formato personalizado
}
\examples{
FechaTexto(as.Date("2023-10-15"))
FechaTexto(as.Date("2023-10-15"), formato = "%d/%m/%Y")
}
