% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{SiError_0}
\alias{SiError_0}
\title{Reemplazar errores de evaluacion por cero}
\usage{
SiError_0(...)
}
\arguments{
  \item{x}{Expresion o valor. Si genera error retorna 0.}
}
\value{Valor numerico o 0 en caso de error.}
\description{
Reemplazar errores de evaluacion por cero
}
\examples{
SiError_0(log(1))
SiError_0(tryCatch(log(-1), warning = function(w) w))
}
