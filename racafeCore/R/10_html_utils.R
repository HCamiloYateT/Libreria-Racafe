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


#' Aplicar formato numerico con estilos HTML condicionales
#'
#' Genera HTML con color y negrita segun cumplimiento de meta.
#'
#' @param x Valor numerico.
#' @param formato Nombre del formato registrado.
#' @param negrita Logico. Aplicar negrita.
#' @param color Color por defecto del texto.
#' @param meta Valor de meta para colorear condicionalmente. `NA` desactiva.
#' @param prop Logico. Si `TRUE`, `x` y `meta` son proporciones (cumplimiento).
#' @return Cadena HTML con el valor formateado.
#' @export
#' @examples
#' FormatearNumero(0.85, "porcentaje", meta = 0.80)
#' FormatearNumero(1250000, "dinero")
FormatearNumero <- function(
    x,
    formato,
    negrita = TRUE,
    color   = "#000000",
    meta    = NA,
    prop    = TRUE) {

  fn    <- ObtenerFormato(formato)
  texto <- fn(x)

  if (!is.na(meta)) {
    cumple <- if (prop) x >= meta else x >= meta
    color <- if (cumple) "#1A7A5E" else "#C0392B"
  }

  estilo <- sprintf(
    "color:%s;%s",
    color,
    if (negrita) "font-weight:600;" else ""
  )

  htmltools::HTML(sprintf('<span style="%s">%s</span>', estilo, texto))
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
