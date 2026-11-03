# Fecha y hora con binding Air Datepicker ----
#' Campo integrado de fecha y hora
#' @inheritParams racafeInputDate
#' @param value Fecha y hora POSIXct, texto interpretable en tz, o NULL.
#' @param tz Zona IANA para interpretar/mostrar; el binding devuelve POSIXct convertible a UTC.
#' @return Campo Air Datepicker con idioma español y fecha/hora integrada; input$id es POSIXct.
#' @export
racafeInputDateTime <- function(id, label = NULL, value = Sys.time(), min = NULL, max = NULL,
                                tz = "America/Bogota", ns = NULL, width = "100%", disabled = FALSE,
                                required = FALSE, help = NULL, error = NULL) {
  .rf_scalar(tz, "tz")
  if (!tz %in% c("UTC", OlsonNames())) stop("tz debe ser una zona IANA v\u00e1lida.")
  parse <- function(x) {
    if (is.null(x)) return(NULL)
    x <- as.POSIXct(x, tz = tz)
    if (length(x) != 1L || is.na(x)) stop("Fecha y hora no v\u00e1lidas.")
    x
  }
  value <- parse(value); min <- parse(min); max <- parse(max)
  if (!is.null(min) && !is.null(max) && as.numeric(min) > as.numeric(max)) stop("min debe ser <= max.")
  if (!is.null(value) && !is.null(min) && as.numeric(value) < as.numeric(min)) value <- min
  if (!is.null(value) && !is.null(max) && as.numeric(value) > as.numeric(max)) value <- max
  # Air Datepicker interpreta la representación local del calendario; no envíe epochs
  # como hora local ni options$tz (shinyWidgets 0.9.1 tiene un fallo en esa rama).
  wall <- function(x) if (is.null(x)) NULL else format(x, "%Y-%m-%d %H:%M:%S", tz = tz)
  tag <- shinyWidgets::airDatepickerInput(.rf_id(id, ns), label, value = wall(value),
    timepicker = TRUE, dateFormat = "dd/MM/yyyy",
    timepickerOpts = shinyWidgets::timepickerOptions(timeFormat = "HH:mm"),
    language = "es", tz = tz, addon = "none", firstDay = 1, minDate = wall(min), maxDate = wall(max), width = width)
  tag <- shiny::tagAppendAttributes(tag, class = "rf-date rf-datetime")
  .rf_input_state(tag, .rf_id(id, ns), disabled = disabled, required = required, help = help, error = error,
                  kind = "datetime", min = min, max = max)
}

# Botones y descarga con autorización de validación ----
#' Actualizar estados de un botón corporativo
#' @param session Sesión Shiny.
#' @param id ID local del botón, sin namespace duplicado.
#' @param disabled,loading,success Estados opcionales. NULL conserva el estado.
#' @return Mensaje al cliente. Estos estados visuales no sustituyen controles de negocio en R.
#' @export
racafeUpdateButton <- function(session = shiny::getDefaultReactiveDomain(), id, disabled = NULL,
                                loading = NULL, success = NULL) {
  for (value in list(disabled, loading, success)) if (!is.null(value)) .rf_flag(value, "estado")
  if (is.null(session)) stop("Se requiere sesi\u00f3n Shiny.")
  prefix <- session$ns("")
  id <- .rf_scalar(id, "id")
  if (!startsWith(id, prefix)) id <- session$ns(id)
  session$sendCustomMessage("racafe-button-state",
                           list(id = id, disabled = disabled, loading = loading, success = success))
}

#' Registrar una descarga que valida antes de empezar y antes de generar contenido
#' @param id ID local de racafeButtonDownload(validate = TRUE).
#' @param validation Objeto racafeFormValidation del formulario.
#' @param filename Nombre o función como en shiny::downloadHandler.
#' @param content Función function(file) o function(file, values). values recibe la instantánea validada.
#' @param session Sesión Shiny actual.
#' @param contentType MIME opcional del archivo.
#' @return Observer de solicitud, invisiblemente; registra `output[[id]]` nativo.
#' @details Usa el enlace visible del botón, sin anchors ocultos. Vuelve a validar en
#'   content para impedir que una llamada directa al enlace omita las reglas de servidor.
#'   La aplicación mantiene permisos y validaciones de negocio adicionales.
#' @export
racafeDownloadHandler <- function(id, validation, filename, content, session = shiny::getDefaultReactiveDomain(),
                                   contentType = NULL) {
  .rf_scalar(id, "id")
  if (is.null(session) || !is.function(content) || !is.function(validation$validate) ||
      !is.function(validation$check)) stop("Se requiere sesi\u00f3n, validador y funci\u00f3n content v\u00e1lidos.")
  session$output[[id]] <- shiny::downloadHandler(filename = filename, contentType = contentType,
    content = function(file) {
      current <- validation$check()
      shiny::req(current$valid)
      if ("values" %in% names(formals(content))) content(file, values = current$values) else content(file)
    })
  observer <- shiny::observeEvent(session$input[[paste0(id, "_request")]], {
    allowed <- isTRUE(validation$validate())
    session$sendCustomMessage("racafe-download-result", list(id = session$ns(id), allowed = allowed))
  }, ignoreInit = TRUE, domain = session)
  invisible(observer)
}
