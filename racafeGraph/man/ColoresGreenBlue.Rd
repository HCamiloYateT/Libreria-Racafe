% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ColoresGreenBlue}
\alias{ColoresGreenBlue}
\title{Paleta gradiente verde-azul segun valores numericos}
\usage{
ColoresGreenBlue(...)
}
\arguments{
  \item{value}{Vector numerico a mapear a colores.}
}
\value{Vector de colores hexadecimales de la misma longitud que `value`.}
\description{
Paleta gradiente verde-azul segun valores numericos
}
\examples{
ColoresGreenBlue(seq(0, 1, length.out = 5))
ColoresGreenBlue(1:10)
}
