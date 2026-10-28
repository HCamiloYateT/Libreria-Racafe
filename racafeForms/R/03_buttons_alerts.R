#' Boton corporativo
#' @param id ID local.
#' @param label Texto; puede ser NULL para boton solo con icono.
#' @param icon Icono Font Awesome de shiny::icon, o NULL.
#' @param variant primary, secondary, success, warning, danger, ghost.
#' @param size xxs, xs, sm, md, lg.
#' @param ns Namespace de modulo.
#' @param title Tooltip accesible.
#' @param disabled Estado inicial deshabilitado.
#' @param ... Argumentos adicionales para racafeButton.
#' @return Etiqueta HTML de botón Shiny; las variantes conservan el ID para `input$id`.
#' @export
racafeButton <- function(id, label, icon = NULL,
                         variant = c("primary", "secondary", "success", "warning", "danger", "ghost"),
                         size = c("sm", "xxs", "xs", "md", "lg"),
                         ns = NULL, title = NULL, disabled = FALSE) {
  variant <- match.arg(variant)
  size <- match.arg(size)
  if (is.null(label) && is.null(icon)) stop("label e icon no pueden ser ambos NULL")
  content <- shiny::tagList(if (!is.null(icon)) .rf_icon(icon),
                            if (!is.null(label)) shiny::tags$span(label))
  btn <- shiny::actionButton(.rf_id(id, ns), label = content,
                             class = .rf_button_class(variant, size))
  if (!is.null(title)) btn <- shiny::tagAppendAttributes(btn, title = title)
  if (isTRUE(disabled)) btn <- shiny::tagAppendAttributes(btn, disabled = "disabled")
  .rf_attach(btn)
}

#' @rdname racafeButton
#' @export
racafeButtonSave <- function(id, label = "Guardar cambios", ns = NULL, ...) {
  racafeButton(id, label, icon = "floppy-disk", variant = "primary", ns = ns, ...)
}
#' @rdname racafeButton
#' @export
racafeButtonCancel <- function(id, label = "Cancelar", ns = NULL, ...) {
  racafeButton(id, label, icon = "xmark", variant = "secondary", ns = ns, ...)
}
#' @rdname racafeButton
#' @export
racafeButtonDelete <- function(id, label = "Eliminar", ns = NULL, ...) {
  racafeButton(id, label, icon = "trash", variant = "danger", ns = ns, ...)
}
#' Boton para downloadHandler (no actionButton)
#' @param id ID local asociado al downloadHandler.
#' @param label Texto.
#' @param icon Icono.
#' @param variant Variante CSS.
#' @param size Tamano.
#' @param ns Namespace.
#' @return Etiqueta HTML de descarga asociada a un `shiny::downloadHandler`.
#' @export
racafeButtonDownload <- function(id, label = "Descargar", icon = "download",
                                 variant = "secondary", size = "sm", ns = NULL) {
  .rf_attach(shiny::downloadButton(.rf_id(id, ns), label = label, icon = .rf_icon(icon),
                                  class = .rf_button_class(variant, size)))
}

#' Mostrar alerta SweetAlert corporativa
#' @param session Sesion Shiny.
#' @param title Titulo.
#' @param text Mensaje de texto plano.
#' @param type success, error, warning, info, question, loading o empty.
#' @param details Detalles técnicos opcionales y explícitos, escapados como texto.
#' @return El resultado de enviar la alerta a la sesión; se usa por su efecto secundario.
#' @export
racafeAlert <- function(session, title, text,
                        type = c("info", "success", "error", "warning", "question", "loading", "empty"),
                        details = NULL) {
  if (identical(type, "e")) type <- "error"
  type <- match.arg(type)
  styled <- !is.null(details) || type %in% c("loading", "empty")
  if (styled) text <- racafeFormMessage(text, if (type == "question") "info" else type, details)
  shinyWidgets::sendSweetAlert(session = session, title = title, text = text,
    type = if (type %in% c("loading", "empty")) NULL else type, width = "420px", html = styled)
}

#' Confirmacion SweetAlert con resultado en input$id
#' @inheritParams racafeAlert
#' @param id ID local de resultado logico.
#' @param confirm_label Texto del boton confirmar.
#' @param cancel_label Texto del boton cancelar.
#' @param ns Namespace opcional; pasar session$ns dentro de modulos.
#' @return El resultado de enviar la confirmación; la respuesta llega como lógico a `input$id`.
#' @export
racafeConfirm <- function(session, id, title, text, type = "warning",
                          confirm_label = "Confirmar", cancel_label = "Cancelar", ns = NULL) {
  shinyWidgets::confirmSweetAlert(
    session = session, inputId = .rf_id(id, ns), title = title, text = text,
    type = type, btn_labels = c(cancel_label, confirm_label),
    btn_colors = c("#6C757D", "#DA291C"), width = "420px", html = FALSE
  )
}
