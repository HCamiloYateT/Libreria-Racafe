# Resúmenes corporativos sin lógica de negocio ----

#' Tabla compacta y escapada para resúmenes
#' @param data data.frame; los textos se escapan, nunca se interpretan como HTML.
#' @param caption Título accesible opcional.
#' @param empty Texto mostrado cuando no hay filas.
#' @param digits Decimales de columnas numéricas; usa punto decimal y coma de miles.
#' @return Contenedor de tabla con desplazamiento local si su contenido no cabe.
#' @export
racafeSummaryTable <- function(data, caption = NULL, empty = "Sin registros", digits = 2L) {
  if (!is.data.frame(data)) stop("data debe ser un data.frame.", call. = FALSE)
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || !digits %in% 0:10) {
    stop("digits debe ser un entero entre 0 y 10.", call. = FALSE)
  }
  if (any(vapply(data, is.list, logical(1)))) stop("No se admiten columnas de listas.", call. = FALSE)
  if (!nrow(data) || !ncol(data)) return(racafeFormMessage(empty, "empty"))
  cells <- lapply(data, function(column) {
    missing <- is.na(column)
    text <- if (is.numeric(column)) {
      formatC(column, digits = digits, format = "f", big.mark = ",", decimal.mark = ".")
    } else as.character(column)
    text[missing] <- ""
    # Quitar la clase HTML evita interpretar contenido de datos como marcado.
    as.vector(text, mode = "character")
  })
  .rf_attach(shiny::tags$div(class = "rf-summary-table", tabindex = "0", role = "region",
    `aria-label` = caption %||% "Resumen tabular",
    shiny::tags$table(class = "rf-table",
      if (!is.null(caption)) shiny::tags$caption(as.character(caption)),
      shiny::tags$thead(shiny::tags$tr(lapply(names(data), function(name) shiny::tags$th(scope = "col", name)))),
      shiny::tags$tbody(lapply(seq_len(nrow(data)), function(i) shiny::tags$tr(
        lapply(seq_along(cells), function(j) shiny::tags$td(
          class = if (is.numeric(data[[j]])) "rf-table-number", cells[[j]][[i]]))))))))
}

#' Total monetario en una fila de etiqueta y valor
#' @param label Etiqueta del total.
#' @param value Valor numérico finito y escalar; no se calcula el total.
#' @param currency Prefijo monetario, por defecto COP.
#' @param digits Número de decimales, de 0 a 10.
#' @return Par semántico etiqueta/valor con formato 1,234.56.
#' @export
racafeMoneyTotal <- function(label, value, currency = "COP", digits = 2L) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    stop("value debe ser un n\u00famero finito.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || !digits %in% 0:10) {
    stop("digits debe ser un entero entre 0 y 10.", call. = FALSE)
  }
  currency <- as.vector(.rf_scalar(currency, "currency", empty = TRUE), mode = "character")
  text <- trimws(paste(currency, formatC(value, format = "f", digits = digits, big.mark = ",", decimal.mark = ".")))
  shiny::tagAppendAttributes(racafeInfoItem(label, text), class = "rf-money-total")
}

#' Separación vertical uniforme de componentes
#' @param ... Tarjetas, grupos u otros componentes Shiny.
#' @param spacing Token de espacio: compact, normal o comfortable.
#' @return Contenedor vertical; no cambia el diseño fuera de sus hijos directos.
#' @export
racafeStack <- function(..., spacing = c("normal", "compact", "comfortable")) {
  .rf_attach(shiny::tags$div(class = paste("rf-stack", paste0("rf-stack--", match.arg(spacing))), ...))
}

#' Lista informativa de coincidencias, advertencias o actividades
#' @param items Vector de textos o lista de componentes Shiny; NULL omite los elementos.
#' @param type info, warning o success; no establece errores de validación.
#' @param title Título opcional.
#' @param font_size Tamaño positivo en píxeles; NULL conserva el tema.
#' @param text_color,accent_color Colores CSS opcionales del texto y borde.
#' @return Sección informativa; los textos se escapan y los componentes HTML explícitos se conservan.
#' @export
#' @param variant default conserva el panel; footnote usa una nota compacta externa.
#' @param swatches Un color CSS por elemento para convenciones.
#' @param inline Presentar los elementos de la nota en una fila adaptable.
racafeNoticeList <- function(items = character(), type = c("info", "warning", "success"), title = NULL,
                              font_size = NULL, text_color = NULL, accent_color = NULL,
                              variant = c("default", "footnote"), swatches = NULL, inline = FALSE) {
  type <- match.arg(type)
  variant <- match.arg(variant)
  .rf_flag(inline, "inline")
  if (!is.null(swatches) && length(swatches) != length(items)) {
    stop("Debe haber un color por elemento.", call. = FALSE)
  }
  if (!is.list(items) && !is.character(items)) stop("items debe ser texto o una lista.", call. = FALSE)
  if (!is.null(font_size) && (!is.numeric(font_size) || length(font_size) != 1L ||
      !is.finite(font_size) || font_size <= 0)) stop("font_size debe ser positivo.", call. = FALSE)
  .color <- function(value, name) {
    if (is.null(value)) return(NULL)
    value <- .rf_scalar(value, name)
    if (grepl("[;{}<>]", value)) stop("Color CSS inválido.", call. = FALSE)
    value
  }
  if (!is.null(swatches)) swatches <- vapply(swatches, .color, character(1), name = "swatches")
  text_color <- .color(text_color, "text_color")
  accent_color <- .color(accent_color, "accent_color")
  style <- paste0(if (!is.null(font_size)) paste0("font-size:", font_size, "px;"),
    if (!is.null(text_color)) paste0("color:", text_color, ";"),
    if (!is.null(accent_color)) paste0("--rf-notice-color:", accent_color, ";"), collapse = "")
  .rf_attach(shiny::tags$section(class = paste("rf-notice-list", paste0("rf-notice-list--", type),
      if (variant == "footnote") "rf-notice-list--footnote", if (inline) "rf-notice-list--inline"),
    style = if (nzchar(style)) style, role = "status",
    if (!is.null(title)) .rf_heading(title, "rf-notice-title", "h4"),
    shiny::tags$ul(lapply(seq_along(items), function(i) if (!is.null(items[[i]])) shiny::tags$li(
      if (!is.null(swatches)) shiny::tags$span(class = "rf-notice-swatch", `aria-hidden` = "true",
        style = paste0("background:", swatches[[i]], ";")), items[[i]])))))
}

#' Modal de confirmación con resumen reutilizable
#' @param id ID local del botón confirmar; recibe un contador Shiny, no un lógico de SweetAlert.
#' @param fields Lista nombrada de valores o contenido para racafeInfoItem; los NULL se omiten.
#' @param ... Contenido adicional, por ejemplo tablas compactas o listas de actividades.
#' @param title Título del modal.
#' @param confirm_label,cancel_label Textos de los botones.
#' @param ns Namespace del módulo; se aplica una vez.
#' @param size Tamaño de racafeModal.
#' @param cancel_id ID local opcional para recibir la cancelación en Shiny; NULL usa modalButton.
#' @return Modal para shiny::showModal; confirme en observeEvent(input$id) y cierre con removeModal.
#' @details Usa racafeInfoGrid y racafeInfoItem. No escribe datos ni registra observers.
#'   racafeConfirm conserva su contrato SweetAlert anterior y no cambia.
#' @export
#' @examples
#' \dontrun{
#' shiny::showModal(racafeConfirmation("confirmar", list(Cliente = "Demo", Opcional = NULL),
#'   racafeSummaryTable(data.frame(Producto = "Café", Cantidad = 2))))
#' }
racafeConfirmation <- function(id, fields = list(), ..., title = "Confirmar", confirm_label = "Confirmar",
                                cancel_label = "Cancelar", ns = NULL, size = "subventana3", cancel_id = NULL) {
  if (!is.list(fields) || (length(fields) && (is.null(names(fields)) || any(!nzchar(names(fields)))))) {
    stop("fields debe ser una lista nombrada.", call. = FALSE)
  }
  fields <- fields[!vapply(fields, is.null, logical(1))]
  items <- lapply(names(fields), function(name) racafeInfoItem(name, fields[[name]]))
  cancel <- if (is.null(cancel_id)) shiny::modalButton(cancel_label) else shiny::tagAppendAttributes(
    racafeButton(cancel_id, cancel_label, variant = "secondary", ns = ns), `data-dismiss` = "modal")
  racafeModal(racafeStack(if (length(items)) do.call(racafeInfoGrid, items), ...), title = title, size = size,
    footer = racafeModalFooter(cancel,
      racafeButton(id, confirm_label, variant = "primary", ns = ns)))
}

