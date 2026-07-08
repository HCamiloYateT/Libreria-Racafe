% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{col_kpi}
\alias{col_kpi}
\title{Colores KPI segun cumplimiento}
\usage{
col_kpi(...)
}
\arguments{
  \item{x}{Vector numerico de cumplimiento (tipicamente proporciones, meta = 1).}
  \item{prop}{Logico. Si `TRUE`, evalua como proporcion respecto a 1.}
}
\value{Vector de colores hexadecimales.}
\description{
Colores KPI segun cumplimiento
}
\examples{
col_kpi(c(0.7, 1.0, 1.2))
}
