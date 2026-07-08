% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{left_join_all}
\alias{left_join_all}
\title{Joins iterativos sobre una lista de data.frames}
\usage{
left_join_all(...)
}
\arguments{
  \item{x}{`data.frame` base.}
  \item{y_list}{Lista de `data.frame`s a unir.}
  \item{by}{Columnas de union (mismo formato que en `dplyr::*_join`).}
  \item{type}{Tipo de join: `"left"`, `"inner"`, `"full"`, `"right"`.}
}
\value{`data.frame` resultado de los joins encadenados.}
\description{
Joins iterativos sobre una lista de data.frames
}
\examples{
base <- data.frame(id = 1:3)
extra1 <- data.frame(id = 1:2, a = c("x", "y"))
extra2 <- data.frame(id = 2:3, b = c("p", "q"))
left_join_all(base, list(extra1, extra2), by = "id")
}
