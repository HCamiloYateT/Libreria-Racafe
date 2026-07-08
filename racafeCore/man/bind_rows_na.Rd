% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{bind_rows_na}
\alias{bind_rows_na}
\title{Combinar data.frames ignorando los vacios}
\usage{
bind_rows_na(...)
}
\arguments{
  \item{...}{`data.frame`s a combinar.}
}
\value{`data.frame` combinado.}
\description{
Equivalente a `dplyr::bind_rows()` pero omite `data.frame`s con
cero filas o `NULL` sin generar advertencias.
}
\examples{
df1 <- data.frame(x = 1:2)
df2 <- data.frame(x = integer(0))
df3 <- data.frame(x = 3:4)
bind_rows_na(df1, df2, df3)
}
