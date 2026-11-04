# OVERLAY DE CONTENEDOR ----

#' Loader corporativo limitado al contenedor
#' @param id ID local del contenedor.
#' @param ... Contenido que conserva su disposición bajo el overlay.
#' @param image Imagen opcional; NULL utiliza el spinner corporativo existente.
#' @param size Tamaño de imagen o spinner en píxeles.
#' @param overlay_color Color CSS del fondo del overlay.
#' @param visible Estado inicial de carga.
#' @param ns Namespace opcional, aplicado una vez.
#' @param triggers IDs de botones o entradas lógicas de confirmación que muestran el overlay
#'   antes de una tarea bloqueante.
#' @return Contenedor relativo con overlay absoluto, sin cubrir el resto de la aplicación.
#' @details Reutiliza los recursos de racafePreloader. Cada operación cierra su overlay mediante
#'   racafeUpdateOverlayLoader(session, id, FALSE), incluso ante error. No depende de idle global:
#'   otras operaciones no lo ocultan. Los triggers muestran antes de que el servidor se bloquee.
#'   Para trabajo asíncrono cierre al resolver y rechazar. La desconexión y errores de outputs del
#'   propio contenedor lo ocultan. En una función use on.exit con la actualización FALSE.
#' @examples
#' racafeOverlayLoader("panel", shiny::p("Contenido"), triggers = "consultar")
#' @export
racafeOverlayLoader <- function(id, ..., image = NULL, size = 96, overlay_color = "rgba(255,255,255,0.88)",
                                 visible = FALSE, ns = NULL, triggers = NULL) {
  id <- .rsh_ui_id(id, ns)
  .rsh_ui_flag(visible, "visible")
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0) stop("size debe ser positivo.")
  overlay_color <- .rsh_ui_text(overlay_color, "overlay_color")
  if (grepl("[;{}<>]", overlay_color)) stop("Color CSS inv\u00e1lido.")
  if (!is.null(image)) image <- .rsh_ui_text(image, "image")
  if (!is.null(triggers)) triggers <- vapply(triggers, .rsh_ui_id, character(1), ns = ns)
  tag <- shiny::tags$div(id = id, class = "rsh-overlay", `aria-busy` = tolower(as.character(visible)),
    `data-rsh-triggers` = as.character(jsonlite::toJSON(
      if (is.null(triggers)) character() else unname(triggers), auto_unbox = FALSE)),
    style = paste0("--rsh-overlay-size:", size, "px;--rsh-overlay-color:", overlay_color, ";"),
    shiny::tags$div(class = "rsh-overlay-content", ...),
    shiny::tags$div(class = "rsh-overlay-cover", hidden = if (!visible) "hidden", role = "status",
      `aria-label` = "Cargando", if (is.null(image)) shiny::tags$span(class = "rsh-loader-spinner") else
        shiny::tags$img(class = "rsh-overlay-image", src = image, alt = "Cargando")))
  htmltools::attachDependencies(tag, list(.rsh_loader_dependency(), .rsh_ui_dependency()))
}

#' Mostrar u ocultar el loader de un contenedor
#' @param session Sesión Shiny del módulo.
#' @param id ID local o ya namespaced del overlay.
#' @param show TRUE muestra; FALSE oculta, también en ramas de error.
#' @return Resultado de enviar el estado al navegador; no altera otros loaders.
#' @export
racafeUpdateOverlayLoader <- function(session, id, show = TRUE) {
  .rsh_ui_flag(show, "show")
  if (is.null(session)) stop("Se requiere una sesi\u00f3n activa.")
  session$sendCustomMessage("rsh-overlay", list(id = .rsh_ui_id(id, session$ns), show = show))
}
