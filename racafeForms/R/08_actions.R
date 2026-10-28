# Acciones agrupadas ----

#' Barra corporativa de acciones
#' @param left,center,right Contenido de los grupos izquierdo, central y derecho; NULL omite el grupo.
#' @param wrap Permitir que los botones pasen a otra línea cuando no hay espacio.
#' @return Contenedor HTML. Conserva botones y enlaces downloadHandler nativos sin simular clics.
#' @export
racafeActionBar <- function(left = NULL, center = NULL, right = NULL, wrap = TRUE) {
  .rf_flag(wrap, "wrap")
  .rf_attach(shiny::tags$div(class = paste("rf-action-bar", if (wrap) "rf-action-bar--wrap"),
    if (!is.null(left)) shiny::tags$div(class = "rf-actions-left", left),
    if (!is.null(center)) shiny::tags$div(class = "rf-actions-center", center),
    if (!is.null(right)) shiny::tags$div(class = "rf-actions-right", right)))
}
