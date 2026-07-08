% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{RecodificarTop}
\alias{RecodificarTop}
\title{Recodificar categorias poco frecuentes (funcion unificada)}
\usage{
RecodificarTop(...)
}
\arguments{
  \item{data}{`data.frame` o `tibble`.}
  \item{var_recode}{Variable categorica a recodificar (tidy-select).}
  \item{var_top}{Variable numerica para el calculo del ranking.}
  \item{fun_Top}{Funcion de resumen: `"sum"`, `"n"` o `"mean"`.}
  \item{estrategia}{`"absoluto"` (top N) o `"relativo"` (porcentaje minimo).}
  \item{n}{Numero de categorias a conservar cuando `estrategia = "absoluto"`.}
  \item{pct_min}{Porcentaje minimo cuando `estrategia = "relativo"` (0-1).}
  \item{nom_var}{Nombre de la nueva columna recodificada.}
  \item{lab_recodificar}{Etiqueta para las categorias agrupadas.}
}
\value{`data.frame` con la nueva columna agregada.}
\description{
Agrupa en una categoria "otros" las categorias que no superan
el umbral definido por la estrategia seleccionada.
}
\examples{
df <- data.frame(
categoria = c("A","A","B","B","C","D"),
valor = c(10, 20, 5, 15, 2, 1)
)
RecodificarTop(df, categoria, valor, "sum", "absoluto",
n = 2, nom_var = "cat_top")
}
