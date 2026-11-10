\name{BGNBD}
\alias{AjustarBGNBD}
\alias{PredecirBGNBD}
\title{Ajuste y predicción condicional BG/NBD}
\description{x contiene Frecuencia, tx y T en semanas. AjustarBGNBD devuelve parámetros, convergencia y diagnóstico; PredecirBGNBD añade ProbActivo y ComprasEsperadas. La selección de población y el valor monetario corresponden al consumidor.}
\usage{
AjustarBGNBD(x)
PredecirBGNBD(x, modelo, semanas)
}
\arguments{
\item{x}{Vector, tabla o figura de entrada, según la función. No se modifica el objeto original.}
\item{modelo}{Modelo ajustado o conjuntos frecuentes producidos por la función correspondiente.}
\item{semanas}{Horizonte predictivo en semanas, la misma unidad de tx y T.}
}
\value{Objeto de presentación, cálculo o contrato reactivo descrito en los detalles de la función.}
