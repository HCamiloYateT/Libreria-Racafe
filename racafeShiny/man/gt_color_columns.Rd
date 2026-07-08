% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{gt_color_columns}
\alias{gt_color_columns}
\title{Colorear columnas especificas en gt con un color fijo}
\usage{
gt_color_columns(...)
}
\arguments{
  \item{gt_table}{Objeto `gt_tbl`.}
  \item{columns}{Columnas a colorear (tidy-select).}
  \item{color}{Color hexadecimal a aplicar.}
}
\value{Objeto `gt_tbl` con columnas coloreadas.}
\description{
Colorear columnas especificas en gt con un color fijo
}
\examples{
\dontrun{
gt_color_columns(gt::gt(head(mtcars)), columns = hp, color = "#28B78D")
}
}
