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
  if (!is.null(args$choices)) args$choices <- .rf_select_choices(args$choices, FALSE)
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
#' Actualizar habilitación real y visual de un control corporativo
#' @param session Sesión Shiny, incluida la de moduleServer.
#' @param id ID local del control, sin namespace duplicado.
#' @param enabled TRUE habilita, FALSE deshabilita. No cambia valores ni permisos de negocio.
#' @return Invisiblemente NULL. El estado se conserva para renderUI posterior en esta sesión/página.
#' @examples
#' \dontrun{
#' shiny::observeEvent(input$activo, {
#'   racafeUpdateEnabled(session, "cuenta", enabled = isTRUE(input$activo))
#' })
#' }
#' @export
racafeUpdateEnabled <- function(session = shiny::getDefaultReactiveDomain(), id, enabled = TRUE) {
  if (is.null(session)) stop("Se requiere una sesi\u00f3n Shiny.")
  .rf_flag(enabled, "enabled")
  prefix <- session$ns("")
  id <- .rf_scalar(id, "id")
  if (!startsWith(id, prefix)) id <- session$ns(id)
  session$sendCustomMessage("racafe-enabled", list(id = id, enabled = enabled))
  invisible(NULL)
}

#' Activar un control corporativo sin inicializar shinyjs
#' @param id ID local.
#' @param session Sesión Shiny actual; resuelve el namespace del módulo.
#' @return Invisiblemente NULL; actualiza el estado real y visual del control.
#' @export
racafeEnable <- function(id, session = shiny::getDefaultReactiveDomain()) racafeUpdateEnabled(session, id, TRUE)
#' @rdname racafeEnable
#' @export
racafeDisable <- function(id, session = shiny::getDefaultReactiveDomain()) racafeUpdateEnabled(session, id, FALSE)
#' @rdname racafeEnable
#' @param condition Lógico; TRUE activa y FALSE desactiva.
#' @export
racafeToggle <- function(id, condition, session = shiny::getDefaultReactiveDomain()) {
  racafeUpdateEnabled(session, id, condition)
}

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
