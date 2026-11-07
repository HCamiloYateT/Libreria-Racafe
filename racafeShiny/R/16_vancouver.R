# TABLAS VANCOUVER ----

#' Tabla gt sobria con encabezado corporativo y totales
#' @param data data.frame; texto escapado y columnas numéricas conservadas.
#' @param header_color Color de la única línea inferior del encabezado.
#' @param header_width Grosor de esa línea en píxeles, por defecto 1.
#' @param background Fondo neutro de la tabla.
#' @param total_background Fondo de fila/columna Total; no se acumula en su intersección.
#' @param total_border_color,total_border_width Color y grosor de la línea superior de la fila Total.
#' @param total_label Etiqueta que identifica totales, ignorando mayúsculas y espacios exteriores.
#' @param total_rows,total_columns Activar el reconocimiento de fila/columna Total.
#' @param align Alineación por nombre de columna o vector completo: left, center, right.
#' @param labels Vector nombrado de etiquetas visibles; conserva los nombres originales para align.
#' @param font_size,row_padding,column_padding Tamaños en píxeles.
#' @param width Ancho CSS; todo el contenedor por defecto.
#' @param min_width Ancho mínimo CSS opcional de la tabla; activa scroll horizontal cuando hace falta.
#' @param max_height Altura máxima opcional del contenedor desplazable.
#' @param decimals Decimales de columnas numéricas; NULL conserva su presentación gt sin reformatear.
#' @param show_header Mostrar encabezados.
#' @param title,subtitle Título y subtítulo opcionales, tratados como texto plano.
#' @param source_note Nota inferior opcional, tratada como texto plano.
#' @return gt_tbl compatible con gt y Shiny; no modifica data ni calcula totales.
#' @details Reutiliza racafeCompactTable y gt_minimal_style en modo compacto. Texto a la izquierda,
#'   cifras a la derecha. Una fila se identifica por su primera celda y una columna por su
#'   encabezado visible. No calcula, añade ni reordena filas. Los colores de totales son opacos y únicos.
#'   Los tamaños/scroll se acotan a esta tabla; las funciones y tablas existentes no cambian.
#' @examples
#' tabla <- data.frame(Producto = c("Café", "Total"), Cantidad = c(12, 12), Total = c(1234.56, 1234.56))
#' racafeTableVancouver(tabla)
#' racafeTableVancouver(tabla, header_color = "#8B1E15", header_width = 3,
#'   align = c(Producto = "left", Cantidad = "center"), min_width = "600px")
#' @export
racafeTableVancouver <- function(data, header_color = "#DA291C", header_width = 1, background = "#FFFFFF",
                                  total_background = "#F5F6F7", total_border_color = "#DDE1E6", total_border_width = 1,
                                  total_label = "Total", total_rows = TRUE, total_columns = TRUE, align = NULL,
                                  labels = NULL, font_size = 12, row_padding = 6, column_padding = 8, width = "100%",
                                  min_width = NULL, max_height = NULL, decimals = 2L, show_header = TRUE,
                                  title = NULL, subtitle = NULL, source_note = NULL) {
  if (!is.data.frame(data) || !ncol(data) || anyDuplicated(names(data)) || any(!nzchar(names(data)))) {
    stop("data debe tener columnas con nombres \u00fanicos y no vac\u00edos.", call. = FALSE)
  }
  for (flag in list(total_rows, total_columns, show_header)) .rsh_ui_flag(flag, "estado")
  colors <- list(header_color, background, total_background, total_border_color)
  for (color in colors) {
    color <- .rsh_ui_text(color, "color")
    if (grepl("[;{}<>]", color)) stop("Color CSS inv\u00e1lido.", call. = FALSE)
  }
  for (value in list(header_width, total_border_width, font_size, row_padding, column_padding)) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
      stop("Los tama\u00f1os deben ser n\u00fameros no negativos.", call. = FALSE)
    }
  }
  if (!is.null(decimals) && (!is.numeric(decimals) || length(decimals) != 1L || is.na(decimals) ||
      !decimals %in% 0:10)) stop("decimals debe ser NULL o un entero entre 0 y 10.", call. = FALSE)
  total_label <- .rsh_ui_text(total_label, "total_label")
  for (unit in list(width, min_width, max_height)) {
    if (!is.null(unit) && any(grepl("[;{}<>]", unit))) stop("Unidad CSS inv\u00e1lida.", call. = FALSE)
  }
  width <- shiny::validateCssUnit(width)
  if (!is.null(min_width)) min_width <- shiny::validateCssUnit(min_width)
  numeric_columns <- vapply(data, is.numeric, logical(1))
  alignment <- stats::setNames(ifelse(numeric_columns, "right", "left"), names(data))
  if (!is.null(align)) {
    if (is.null(names(align))) {
      if (length(align) != ncol(data)) stop("align debe corresponder a las columnas.", call. = FALSE)
      names(align) <- names(data)
    }
    if (anyNA(align) || anyDuplicated(names(align)) || !all(names(align) %in% names(data)) ||
        !all(align %in% c("left", "center", "right"))) stop("align inv\u00e1lido.", call. = FALSE)
    alignment[names(align)] <- align
  }
  column_labels <- stats::setNames(names(data), names(data))
  if (!is.null(labels)) {
    if (!is.character(labels) || is.null(names(labels)) || anyNA(labels) || anyDuplicated(names(labels)) ||
        !all(names(labels) %in% names(data))) stop("labels debe nombrar columnas existentes.", call. = FALSE)
    column_labels[names(labels)] <- .rsh_plain(labels)
  }
  table <- racafeCompactTable(data, backend = "gt", align = alignment, transparent = FALSE,
    font_size = font_size, row_padding = row_padding, max_height = max_height, show_header = show_header)
  options <- list(table.width = width, table.background.color = background, table.layout = "auto",
    column_labels.background.color = background, column_labels.font.weight = "600",
    column_labels.border.bottom.style = "solid", column_labels.border.bottom.width = gt::px(header_width),
    column_labels.border.bottom.color = header_color, data_row.padding.horizontal = gt::px(column_padding),
    column_labels.padding = gt::px(row_padding), column_labels.padding.horizontal = gt::px(column_padding),
    container.width = "100%", container.padding.x = gt::px(0), container.padding.y = gt::px(0),
    container.overflow.x = "auto", container.overflow.y = "auto")
  table <- do.call(gt::tab_options, c(list(data = table), options))
  if (nrow(data)) {
    table <- gt::cols_label(table, .list = as.list(column_labels))
    if (!is.null(decimals) && any(numeric_columns)) table <- gt::fmt_number(table,
      columns = tidyselect::all_of(names(data)[numeric_columns]), decimals = decimals, sep_mark = ",", dec_mark = ".")
    total_key <- tolower(trimws(total_label))
    rows <- which(!is.na(data[[1]]) & tolower(trimws(as.character(data[[1]]))) == total_key)
    columns <- names(data)[tolower(trimws(column_labels)) == total_key]
    if (total_columns && length(columns)) table <- gt::tab_style(table,
      style = gt::cell_fill(color = total_background),
      locations = list(gt::cells_body(columns = tidyselect::all_of(columns)),
        gt::cells_column_labels(columns = tidyselect::all_of(columns))))
    if (total_rows && length(rows)) table <- gt::tab_style(table,
      style = list(gt::cell_text(weight = "bold"), gt::cell_fill(color = total_background),
        gt::cell_borders(sides = "top", color = total_border_color, weight = gt::px(total_border_width))),
      locations = gt::cells_body(rows = rows))
  }
  if (!is.null(title) || !is.null(subtitle)) table <- gt::tab_header(table,
    title = if (!is.null(title)) .rsh_plain(title), subtitle = if (!is.null(subtitle)) .rsh_plain(subtitle))
  attr(table, "rsh_vancouver_note") <- source_note
  attr(table, "rsh_vancouver_min_width") <- min_width
  class(table) <- unique(c("rsh_vancouver_gt", class(table)))
  table
}

#' Renderizado acotado de tablas Vancouver
#' @param x Tabla de racafeTableVancouver.
#' @param ... Argumentos de as.tags.
#' @return Contenedor gt con scroll horizontal y ancho mínimo opcional.
#' @method as.tags rsh_vancouver_gt
#' @importFrom htmltools as.tags
#' @export
#' @keywords internal
as.tags.rsh_vancouver_gt <- function(x, ...) {
  tag <- NextMethod()
  tag$attribs$style <- paste0(tag$attribs$style, ";max-width:100%;min-width:0;")
  minimum <- attr(x, "rsh_vancouver_min_width")
  if (!is.null(minimum)) {
    tag$attribs$tabindex <- "0"
    tag$attribs$`aria-label` <- "Tabla con desplazamiento horizontal"
    tag$children <- c(tag$children, list(shiny::tags$style(htmltools::HTML(paste0(
      "#", tag$attribs$id, " .gt_table {min-width:", minimum, ";}")))))
  }
  note <- attr(x, "rsh_vancouver_note")
  if (is.null(note)) tag else shiny::tagList(tag, racafeTableNote(note))
}

