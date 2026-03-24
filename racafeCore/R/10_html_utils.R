# ============================================================
# Seccion 10 — Utilidades HTML
# Helpers para generacion de HTML en contextos Shiny/rmarkdown
# ============================================================


#' Generar saltos de linea HTML
#'
#' @param n Numero de saltos de linea `<br>` a insertar.
#' @return Objeto HTML de Shiny.
#' @export
#' @examples
#' Saltos(2)
Saltos <- function(n = 1) {
  n <- max(1L, as.integer(n))
  htmltools::HTML(strrep("<br>", n))
}


#' Insertar espacios no separables HTML
#'
#' @param n Numero de entidades `&nbsp;` a insertar.
#' @return Objeto HTML de Shiny.
#' @export
#' @examples
#' Espacios(3)
Espacios <- function(n = 1) {
  n <- max(1L, as.integer(n))
  htmltools::HTML(strrep("&nbsp;", n))
}


#' Marcar texto como campo obligatorio con asterisco rojo
#'
#' @param s Texto de la etiqueta.
#' @return Objeto HTML con asterisco rojo al final.
#' @export
#' @examples
#' Obligatorio("Nombre del cliente")
Obligatorio <- function(s) {
  htmltools::HTML(
    sprintf('%s<span style="color:#C0392B;margin-left:3px;">*</span>', s)
  )
}
