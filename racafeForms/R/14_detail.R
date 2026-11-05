# DETALLE Y ACTIVIDADES ----

#' Modal corporativo para consulta y actualización
#' @param title Título del modal.
#' @param ... Formularios, tarjetas, tablas o contenido Shiny.
#' @param metadata Contenido informativo opcional; se conserva sin asumir estructura de datos.
#' @param footer NULL incluye Cerrar; acciones y botones son responsabilidad del consumidor.
#' @param size Tamaño de racafeModal; subventana2 por defecto.
#' @param scrollable Limitar y desplazar verticalmente el cuerpo.
#' @param easy_close Permitir cierre nativo con Escape o clic fuera.
#' @return Modal para showModal, construido con racafeModal y racafeModalFooter.
#' @export
#' @examples
#' \dontrun{
#' shiny::showModal(racafeModalDetail("Detalle", racafeInfoItem("Cliente", "Ejemplo")))
#' }
racafeModalDetail <- function(title, ..., metadata = NULL, footer = NULL, size = "subventana2",
                               scrollable = TRUE, easy_close = FALSE) {
  .rf_flag(scrollable, "scrollable")
  content <- racafeStack(if (!is.null(metadata)) shiny::tags$div(class = "rf-detail-metadata", metadata), ...)
  modal <- racafeModal(content, title = title, size = size, easy_close = easy_close,
    footer = if (is.null(footer)) racafeModalFooter() else footer)
  shiny::tagAppendAttributes(modal,
    class = if (scrollable) "rf-modal-detail rf-modal-detail--scroll" else "rf-modal-detail")
}

#' Badge de etiqueta y valor reutilizando el badge semántico
#' @param label Etiqueta de texto plano.
#' @param value Valor escalar de texto plano.
#' @param variant,size Opciones de racafeStatusBadge.
#' @return Badge informativo escapado, sin reglas de negocio.
#' @examples
#' racafeDataBadge("Responsable", "Ana")
#' @export
racafeDataBadge <- function(label, value, variant = "neutral", size = "sm") {
  label <- .rf_scalar(label, "label")
  if (!is.atomic(value) || length(value) != 1L || is.na(value)) stop("value debe ser escalar no NA.")
  shiny::tagAppendAttributes(racafeStatusBadge(paste0(label, ": ", as.character(value)), variant, size),
                            class = "rf-data-badge")
}

#' Sección informativa de detalle
#' @param title Título del grupo.
#' @param ... Contenido Shiny.
#' @return Grupo corporativo sin duplicar la infraestructura de racafeFormGroup.
#' @examples
#' racafeDetailSection("Cliente", racafeInfoItem("NIT", "900123456"))
#' @export
racafeDetailSection <- function(title, ...) racafeFormGroup(..., title = title)

#' Etiqueta de fecha opcional
#' @param value Date, POSIXt o texto convertible a Date; escalar o NULL.
#' @param format Formato R de fecha.
#' @param empty Texto cuando no hay una fecha válida.
#' @return span con la fecha escapada; no modifica datos ni la zona horaria recibida.
#' @examples
#' racafeDateLabel(as.Date("2026-11-04"))
#' @export
racafeDateLabel <- function(value = NULL, format = "%d/%m/%Y", empty = "Sin fecha") {
  text <- empty
  if (length(value) > 1L) stop("value debe ser escalar.")
  if (length(value) && !is.na(value)) {
    date <- tryCatch(if (inherits(value, c("Date", "POSIXt"))) value else as.Date(value), error = function(e) NA)
    if (!is.na(date)) text <- base::format(date, format = format)
  }
  .rf_attach(shiny::tags$span(class = "rf-date-label", as.vector(as.character(text), mode = "character")))
}

#' Checklist de actividades con claves estables
#' @param id ID local del input.
#' @param items Vector de textos; nombres únicos identifican las actividades. Sin nombres usa posiciones como claves.
#' @param checked Claves seleccionadas o vector lógico paralelo. Un lógico nombrado se alinea por clave.
#' @param ns Namespace del módulo, aplicado una vez.
#' @return Grupo de casillas nativo Shiny. input$id contiene claves de actividades, nunca posiciones ambiguas.
#' @details No registra observers ni persistencia. checked debe corresponder exactamente a las claves declaradas.
#' @examples
#' racafeChecklist("actividades", c(a = "Revisar", b = "Confirmar"), checked = c(b = TRUE, a = FALSE))
#' @export
racafeChecklist <- function(id, items, checked = character(), ns = NULL) {
  if (!is.character(items) || anyNA(items)) stop("items debe ser un vector de textos sin NA.")
  keys <- if (is.null(names(items))) as.character(seq_along(items)) else names(items)
  if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) {
    stop("Las claves deben ser \u00fanicas y no vac\u00edas.")
  }
  if (is.logical(checked)) {
    if (anyNA(checked) || length(checked) != length(keys)) stop("checked debe corresponder a todas las actividades.")
    if (!is.null(names(checked))) {
      if (anyDuplicated(names(checked)) || !setequal(names(checked), keys)) stop("Claves de checked inv\u00e1lidas.")
      checked <- checked[keys]
    }
    checked <- keys[checked]
  }
  if (!is.character(checked) || anyNA(checked) || !all(checked %in% keys)) stop("checked contiene claves desconocidas.")
  choices <- stats::setNames(keys, as.vector(items, mode = "character"))
  tag <- shiny::checkboxGroupInput(.rf_id(id, ns), label = NULL, choices = choices, selected = checked)
  .rf_input_state(shiny::tagAppendAttributes(tag, class = "rf-checklist"), .rf_id(id, ns), kind = "select")
}
