# Modales corporativos ----

#' Modal Shiny con tamaños corporativos definidos en Compartido
#' @param ... Contenido del modal.
#' @param title Título opcional; NULL omite el encabezado.
#' @param size Clase existente: ventana, subventana1 a subventana5, o aviso.
#' @param footer Pie opcional; NULL omite el pie.
#' @param easy_close Permitir cierre con Escape o clic fuera. FALSE requiere una acción de cierre explícita.
#' @return Modal para pasar a shiny::showModal. Requiere racafeSharedAssets; no duplica reglas de ancho.
#' @export
#' @examples
#' \dontrun{
#' shiny::showModal(racafeModal("Contenido", title = "Detalle", size = "aviso"))
#' }
racafeModal <- function(..., title = NULL,
                          size = c("subventana3", "ventana", "subventana1", "subventana2",
                                   "subventana4", "subventana5", "aviso"),
                          footer = racafeModalFooter(), easy_close = FALSE) {
  size <- match.arg(size)
  .rf_flag(easy_close, "easy_close")
  .rf_attach(shiny::tagAppendAttributes(
    shiny::modalDialog(..., title = title, footer = footer, size = "l", easyClose = easy_close),
    class = paste("rf-modal", size)))
}

#' Pie de modal con grupos de acciones
#' @param ... Acciones situadas a la derecha. Sin acciones se incluye Cerrar nativo de Shiny.
#' @param left,center,right Grupos opcionales, como en racafeActionBar.
#' @return Pie HTML con botones nativos y apariencia corporativa.
#' @export
racafeModalFooter <- function(..., left = NULL, center = NULL, right = NULL) {
  actions <- list(...)
  if (!length(actions) && is.null(left) && is.null(center) && is.null(right)) {
    actions <- list(shiny::tagAppendAttributes(shiny::modalButton("Cerrar"), class = "rf-btn rf-btn--secondary"))
  }
  shiny::tags$div(class = "rf-modal-footer",
    racafeActionBar(left, center, right = shiny::tagList(right, actions)))
}
