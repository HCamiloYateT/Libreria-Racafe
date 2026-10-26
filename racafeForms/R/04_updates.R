#' Actualizar selector corporativo
#' @param session Sesion del modulo que contiene el input.
#' @param id ID local (NO pasar ns(id)).
#' @param ... Parametros del update nativo.
#' @param value Nuevo valor del control.
#' @return El resultado del actualizador nativo; se usa por su efecto secundario en la sesión.
#' @export
racafeUpdateSelect <- function(session, id, ...) {
  shinyWidgets::updatePickerInput(session, inputId = id, ...)
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
