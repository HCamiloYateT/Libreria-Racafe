% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{FormatearNumero}
\alias{FormatearNumero}
\title{Aplicar formato numerico con estilos HTML condicionales}
\usage{
FormatearNumero(...)
}
\arguments{
  \item{x}{Valor numerico.}
  \item{formato}{Nombre del formato registrado.}
  \item{negrita}{Logico. Aplicar negrita.}
  \item{color}{Color por defecto del texto.}
  \item{meta}{Valor de meta para colorear condicionalmente. `NA` desactiva.}
  \item{prop}{Logico. Si `TRUE`, `x` y `meta` son proporciones (cumplimiento).}
}
\value{Cadena HTML con el valor formateado.}
\description{
Genera HTML con color y negrita segun cumplimiento de meta.
}
\examples{
FormatearNumero(0.85, "porcentaje", meta = 0.80)
FormatearNumero(1250000, "dinero")
}
