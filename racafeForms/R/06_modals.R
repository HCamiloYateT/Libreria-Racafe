# Modales corporativos ----

#' Modal Shiny con tamaños corporativos definidos en Compartido
#' @param ... Contenido del modal.
#' @param title Título opcional; NULL omite el encabezado.
#' @param size Clase existente: ventana, subventana1 a subventana5, o aviso.
#' @param footer Pie opcional; NULL omite el pie.
#' @param easy_close Permitir cierre con Escape o clic fuera. FALSE requiere una acción de cierre explícita.
#' @return Modal para shiny::showModal con tamaños corporativos incluidos en la dependencia base.
#' @details Precargue racafeFormsDependencies en la UI inicial. Si el primer componente se genera
#'   desde showModal, espera a sus CSS antes de ejecutar la apertura nativa, evitando saltos de ancho.
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
  if (!is.null(footer) && !(inherits(footer, "shiny.tag") &&
      grepl("(^| )rf-modal-footer( |$)", footer$attribs$class %||% ""))) footer <- racafeModalFooter(footer)
  tag <- shiny::modalDialog(..., title = title, footer = footer, size = "l", easyClose = easy_close)
  last <- length(tag$children)
  native <- as.character(tag$children[[last]]$children[[1]])
  gate <- paste0('(function () {',
    'var modal = document.getElementById("shiny-modal"), start = Date.now();',
    'var showNative = function () {', native, '};function ready() {',
    'if (!modal || !modal.isConnected || document.getElementById("shiny-modal") !== modal) return;',
    'var style = getComputedStyle(document.documentElement);',
    'if (style.getPropertyValue("--rf-forms-ready").trim() === "1" && ',
    'style.getPropertyValue("--rf-modal-sizes-ready").trim() === "1") {showNative();}',
    'else if (Date.now() - start < 15000) requestAnimationFrame(ready);',
    'else if (window.Shiny && Shiny.notifications) Shiny.notifications.show({',
    'html:"No se pudieron cargar los estilos del modal. Recargue la aplicaci\u00f3n.",type:"error"});',
    '} ready();}());')
  tag$children[[last]] <- shiny::tags$script(htmltools::HTML(gate))
  .rf_attach(shiny::tagAppendAttributes(tag, class = paste("rf-modal", size)))
}

#' Pie de modal con grupos de acciones
#' @param ... Acciones situadas a la derecha. Sin acciones se incluye Cerrar nativo de Shiny.
#' @param left,center,right Grupos opcionales; conservan contenido y orden, alineados
#'   a la derecha dentro del footer, también en pantallas pequeñas.
#' @return Pie HTML con botones nativos normalizados a rf-btn y acciones horizontales a la derecha.
#' @details Conserva IDs, handlers, estados y data-dismiss. Cerrar/Cancelar son secundarios;
#'   confirmar/guardar/aceptar y botones sin variante son principales. Otras variantes rf explícitas se conservan.
#' @export
racafeModalFooter <- function(..., left = NULL, center = NULL, right = NULL) {
  actions <- list(...)
  if (!length(actions) && is.null(left) && is.null(center) && is.null(right)) {
    actions <- list(shiny::tagAppendAttributes(shiny::modalButton("Cerrar"), class = "rf-btn rf-btn--secondary"))
  }
  .rf_attach(shiny::tags$div(class = "rf-modal-footer",
    racafeActionBar(.rf_modal_actions(left), .rf_modal_actions(center),
                   right = .rf_modal_actions(shiny::tagList(right, actions)))))
}

# Normalizar solo botones/enlaces Shiny del footer, conservando sus bindings ----
.rf_modal_actions <- function(node) {
  if (inherits(node, "shiny.tag")) {
    classes <- strsplit(node$attribs$class %||% "", "\\s+", perl = TRUE)[[1]]
    if (node$name == "button" || (node$name == "a" && any(classes %in% c("btn", "rf-btn", "shiny-download-link")))) {
      if (is.null(node$attribs$`data-rf-inactive`)) {
        disabled <- !is.null(node$attribs$disabled) && !isFALSE(node$attribs$disabled)
        node$attribs$`data-rf-inactive` <- tolower(as.character(disabled))
      }
      text <- tolower(trimws(.rf_text_content(node)))
      dismiss <- node$attribs$`data-dismiss` %||% node$attribs$`data-bs-dismiss`
      cancel <- identical(dismiss, "modal") || grepl("^(cancelar|cancel|cerrar|close|dismiss|volver)( |$)", text)
      confirm <- grepl("^(confirmar|guardar|aceptar|confirm|save|accept|continuar|aplicar)( |$)", text)
      variant <- classes[grepl("^rf-btn--(primary|secondary|success|warning|danger|ghost)$", classes)]
      variant <- if (cancel) "rf-btn--secondary" else if (confirm || !length(variant)) "rf-btn--primary" else variant[1]
      pattern <- paste0("^(rf-btn--(primary|secondary|success|warning|danger|ghost)|",
                        "btn-(default|primary|secondary|success|warning|danger|light|dark))$")
      classes <- classes[!grepl(pattern, classes)]
      if (!any(grepl("^rf-btn--(xxs|xs|sm|md|lg)$", classes))) classes <- c(classes, "rf-btn--sm")
      node$attribs$class <- paste(unique(c(classes[nzchar(classes)], "rf-btn", variant)), collapse = " ")
    }
    node$children <- lapply(node$children, .rf_modal_actions)
  } else if (is.list(node)) node[] <- lapply(node, .rf_modal_actions)
  node
}
