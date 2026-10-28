#' Actualizar selector corporativo
#' @param session Sesion del modulo que contiene el input.
#' @param id ID local (NO pasar ns(id)).
#' @param ... Parametros del update nativo.
#' @param value Nuevo valor del control.
#' @details Al cambiar `choices` sin indicar `selected`, conserva los valores válidos
#'   de la sesión, incluida una selección vacía. `selected` explícito tiene prioridad.
#' @return El resultado del actualizador nativo; se usa por su efecto secundario en la sesión.
#' @export
racafeUpdateSelect <- function(session, id, ...) {
  args <- list(...)
  if (!is.null(args$choices) && !"selected" %in% names(args)) {
    actuales <- session$input[[id]]
    args$selected <- intersect(as.character(actuales), as.character(.rf_choice_values(args$choices)))
  }
  do.call(shinyWidgets::updatePickerInput, c(list(session = session, inputId = id), args))
}
#' Actualizar numerico corporativo
#' @rdname racafeUpdateSelect
#' @export
racafeUpdateNumeric <- function(session, id, value = NULL, ...) {
  shinyWidgets::updateAutonumericInput(session, inputId = id, value = value, ...)
}
#' Actualizar fecha corporativa
#' @rdname racafeUpdateSelect
#' @export
racafeUpdateDate <- function(session, id, ...) {
  shinyWidgets::updateAirDateInput(session, inputId = id, ...)
}
#' Actualizar rango de fechas corporativo
#' @rdname racafeUpdateSelect
#' @export
racafeUpdateDateRange <- function(session, id, ...) {
  shiny::updateDateRangeInput(session, inputId = id, ...)
}
#' Actualizar switch corporativo
#' @rdname racafeUpdateSelect
#' @export
racafeUpdateSwitch <- function(session, id, value) {
  shinyWidgets::updateMaterialSwitch(session, inputId = id, value = value)
}
#' Activar un input usando shinyjs
#' @param id ID local.
#' @return El resultado de la llamada a shinyjs; se usa para modificar el estado del control.
#' @export
racafeEnable <- function(id) shinyjs::enable(id)
#' @rdname racafeEnable
#' @export
racafeDisable <- function(id) shinyjs::disable(id)
#' @rdname racafeEnable
#' @param condition Logico; TRUE activa y FALSE desactiva.
#' @export
racafeToggle <- function(id, condition) shinyjs::toggleState(id, condition = condition)

# Restablecimiento acotado a un formulario ----

#' Restablecer o limpiar los campos de un formulario
#' @param session Sesión Shiny del módulo.
#' @param id ID local asignado a racafeForm.
#' @param clear TRUE limpia valores editables; FALSE restaura su valor inicial en la UI.
#' @return Invisiblemente NULL. El navegador informa success, count, failures y request_id en `input$<id>_reset_result`.
#' @details Requiere racafeSharedAssets. Conserva los bindings de texto, selectores, fechas y numéricos.
#'   No modifica botones, archivos ni campos readonly. Cada instancia dinámica tiene su propia instantánea.
#'   No ejecuta lógica de negocio ni envía los valores de la instantánea en el resultado.
#' @export
racafeResetForm <- function(session, id, clear = FALSE) {
  .rf_flag(clear, "clear")
  session$sendCustomMessage("rf-form-reset", list(id = session$ns(.rf_id(id)), clear = clear))
  invisible(NULL)
}
