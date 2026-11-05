# LISTAS Y TABLAS CORPORATIVAS ----
.rsh_ui_id <- function(id, ns = NULL) {
  id <- .rsh_ui_text(id, "id")
  if (!is.null(ns) && !is.function(ns)) stop("ns debe ser una funci\u00f3n.", call. = FALSE)
  if (is.null(ns) || startsWith(id, ns(""))) id else ns(id)
}
.rsh_plain <- function(value) {
  if (!is.atomic(value)) stop("El contenido debe ser texto o un vector at\u00f3mico.", call. = FALSE)
  as.vector(as.character(value), mode = "character")
}

#' Elemento corporativo de lista con selección Shiny opcional
#' @param title Título de texto plano, escapado.
#' @param subtitle Texto secundario opcional.
#' @param metadata Vector atómico opcional; los nombres se muestran como etiquetas.
#' @param badges Textos o etiquetas Shiny explícitas para estados; el texto se escapa.
#' @param icon Nombre de icono o etiqueta Shiny opcional.
#' @param value Valor JSON de la selección; escalar o lista de claves, sin código JavaScript.
#' @param id ID del input de selección. Varios elementos pueden compartirlo sin repetir IDs HTML.
#' @param ns Namespace del módulo; se aplica una sola vez.
#' @param selected,disabled,compact Estados lógicos del elemento.
#' @return Etiqueta de lista. Con id y value, input$id recibe value al hacer clic, Enter o Espacio.
#' @details Sin id conserva value como metadato para que MenuHeaderServer gestione la selección.
#'   No anide otros botones en un elemento seleccionable; use contenido independiente del grupo.
#' @examples
#' racafeListItem("Cliente de ejemplo", metadata = c(NIT = "900123456"), id = "cliente", value = "c1")
#' @export
racafeListItem <- function(title, subtitle = NULL, metadata = NULL, badges = NULL, icon = NULL,
                            value = NULL, id = NULL, ns = NULL, selected = FALSE, disabled = FALSE, compact = TRUE) {
  for (flag in list(selected, disabled, compact)) .rsh_ui_flag(flag, "estado")
  title <- .rsh_ui_text(title, "title")
  if (!is.null(id) && is.null(value)) stop("value es obligatorio cuando se utiliza id.", call. = FALSE)
  if (!is.null(id)) id <- .rsh_ui_id(id, ns)
  if (is.character(icon)) icon <- shiny::icon(.rsh_ui_text(icon, "icon"))
  if (!is.null(icon) && !inherits(icon, "shiny.tag")) stop("icon debe ser nombre o etiqueta.", call. = FALSE)
  if (is.character(badges)) badges <- .rsh_plain(badges)
  payload <- if (!is.null(value)) as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null"))
  meta_text <- if (!is.null(metadata)) .rsh_plain(metadata)
  meta_names <- names(metadata)
  tag <- shiny::tags$div(class = paste("rsh-list-item", if (compact) "rsh-list-item--compact",
    if (selected) "rsh-list-item--selected", if (disabled) "rsh-list-item--disabled"),
    role = if (is.null(id)) "listitem" else "button", tabindex = if (!is.null(id) && !disabled) "0",
    `aria-disabled` = tolower(as.character(disabled)),
    `aria-pressed` = if (!is.null(id)) tolower(as.character(selected)),
    `data-rsh-input` = id, `data-rsh-value` = payload,
    icon, shiny::tags$div(class = "rsh-list-copy",
      shiny::tags$strong(class = "rsh-list-title", title),
      if (!is.null(subtitle)) shiny::tags$span(class = "rsh-list-subtitle", .rsh_plain(subtitle)),
      if (length(meta_text)) shiny::tags$div(class = "rsh-list-metadata", lapply(seq_along(meta_text), function(i) {
        label <- if (!is.null(meta_names) && nzchar(meta_names[[i]])) paste0(meta_names[[i]], ": ") else ""
        shiny::tags$span(paste0(label, meta_text[[i]]))
      }))),
    if (!is.null(badges)) shiny::tags$div(class = "rsh-list-badges", badges))
  htmltools::attachDependencies(tag, .rsh_ui_dependency())
}

#' Grupo adaptable y desplazable de elementos
#' @param ... Elementos Shiny; NULL se omite.
#' @param max_height Altura máxima CSS; NULL sin límite.
#' @param compact,dividers Densidad y divisorias entre elementos.
#' @param empty_message Texto escapado del estado vacío.
#' @return Contenedor con rol list y dependencia visual encapsulada.
#' @examples
#' racafeListGroup(racafeListItem("Ejemplo"), max_height = "300px")
#' @export
racafeListGroup <- function(..., max_height = "500px", compact = TRUE, empty_message = "Sin registros",
                             dividers = TRUE) {
  .rsh_ui_flag(compact, "compact"); .rsh_ui_flag(dividers, "dividers")
  items <- Filter(.rsh_has_content, list(...))
  tag <- shiny::tags$div(class = paste("rsh-list-group",
    if (compact) "rsh-list-group--compact" else "rsh-list-group--normal",
    if (dividers) "rsh-list-group--dividers"), role = "list",
    style = if (!is.null(max_height)) paste0("--rsh-list-max-height:", shiny::validateCssUnit(max_height), ";"),
    if (length(items)) items else shiny::tags$p(class = "rsh-list-empty", .rsh_plain(empty_message)))
  htmltools::attachDependencies(tag, .rsh_ui_dependency())
}

#' Tabla compacta de presentación para navbar y paneles
#' @param data data.frame; por defecto todo el contenido se escapa.
#' @param backend gt devuelve gt_tbl; html devuelve etiquetas Shiny.
#' @param show_header,striped,bordered,transparent Opciones de presentación.
#' @param font_size,row_padding Tamaño de fuente y padding vertical en píxeles.
#' @param max_height Altura máxima CSS opcional para HTML; gt usa su contenedor nativo.
#' @param align Vector de left, center o right por columna, opcionalmente nombrado.
#' @param html_columns Nombres de columnas de HTML confiable declarado explícitamente. No sanea HTML no confiable.
#' @return gt_tbl o contenedor HTML, según backend. Los datos vacíos muestran Sin registros.
#' @examples
#' racafeCompactTable(data.frame(Cliente = "Café", Total = 1234.56), backend = "html")
#' @export
racafeCompactTable <- function(data, backend = c("gt", "html"), show_header = TRUE, striped = FALSE,
                                bordered = FALSE, transparent = TRUE, font_size = 11, row_padding = 2,
                                max_height = NULL, align = NULL, html_columns = character()) {
  backend <- match.arg(backend)
  if (!is.data.frame(data) || any(vapply(data, is.list, logical(1)))) stop("data debe ser un data.frame at\u00f3mico.")
  for (flag in list(show_header, striped, bordered, transparent)) .rsh_ui_flag(flag, "estado")
  for (size in list(font_size, row_padding)) {
    if (!is.numeric(size) || length(size) != 1L || is.na(size) || !is.finite(size) || size < 0) {
      stop("Tama\u00f1o inv\u00e1lido.")
    }
  }
  if (!all(html_columns %in% names(data))) stop("html_columns debe nombrar columnas de data.")
  alignment <- stats::setNames(rep("left", ncol(data)), names(data))
  if (!is.null(align)) {
    if (is.null(names(align))) {
      if (length(align) != ncol(data)) stop("align debe corresponder a las columnas.")
      names(align) <- names(data)
    }
    if (!all(names(align) %in% names(data)) || !all(align %in% c("left", "center", "right"))) {
      stop("align inv\u00e1lido.")
    }
    alignment[names(align)] <- align
  }
  if (backend == "gt") {
    if (!nrow(data) || !ncol(data)) {
      data <- data.frame(Mensaje = "Sin registros")
      show_header <- FALSE; alignment <- c(Mensaje = "left"); html_columns <- character()
    }
    data[] <- lapply(data, function(column) if (is.character(column)) .rsh_plain(column) else column)
    table <- gt::gt(data)
    if (length(html_columns)) table <- gt::fmt_markdown(table, columns = tidyselect::all_of(html_columns))
    table <- gt_minimal_style(table, compact = TRUE, show_header = show_header, striped = striped,
      bordered = bordered, transparent = transparent, font_size = font_size, row_padding = row_padding)
    for (column in names(alignment)) {
      table <- gt::cols_align(table, align = alignment[[column]], columns = tidyselect::all_of(column))
    }
    if (!nrow(data)) table <- gt::tab_options(table, table.font.size = gt::px(font_size))
    if (!is.null(max_height)) attr(table, "rsh_max_height") <- shiny::validateCssUnit(max_height)
    class(table) <- unique(c("rsh_compact_gt", class(table)))
    return(table)
  }
  cells <- lapply(names(data), function(name) {
    text <- .rsh_plain(data[[name]]); text[is.na(data[[name]])] <- ""
    lapply(text, function(value) if (name %in% html_columns) htmltools::HTML(value) else value)
  })
  table <- shiny::tags$table(
    if (show_header && ncol(data)) shiny::tags$thead(shiny::tags$tr(lapply(names(data), function(name) {
      shiny::tags$th(scope = "col", class = paste0("rsh-align-", alignment[[name]]), name)
    }))),
    shiny::tags$tbody(if (!nrow(data) || !ncol(data)) shiny::tags$tr(shiny::tags$td(
      colspan = max(1L, ncol(data)), "Sin registros")) else lapply(seq_len(nrow(data)), function(i) shiny::tags$tr(
        lapply(seq_along(cells), function(j) shiny::tags$td(
          class = paste0("rsh-align-", alignment[[j]]), cells[[j]][[i]]))))))
  tag <- shiny::tags$div(class = paste("rsh-compact-table", if (striped) "rsh-table--striped",
    if (bordered) "rsh-table--bordered", if (transparent) "rsh-table--transparent"),
    style = paste0("--rsh-table-font:", font_size, "px;--rsh-table-padding:", row_padding, "px;",
      if (!is.null(max_height)) paste0("max-height:", shiny::validateCssUnit(max_height), ";")), table)
  htmltools::attachDependencies(tag, .rsh_ui_dependency())
}

# REFERENCIAS ACCESIBLES DE TABLAS GT ----
#' Renderizar una tabla compacta con encabezados únicos por tabla
#' @param x Objeto de racafeCompactTable con backend gt.
#' @param ... Argumentos de as.tags.
#' @return Etiquetas gt con IDs de encabezado y referencias headers coherentes.
#' @importFrom htmltools as.tags
#' @method as.tags rsh_compact_gt
#' @export
#' @keywords internal
as.tags.rsh_compact_gt <- function(x, ...) {
  tag <- NextMethod()
  max_height <- attr(x, "rsh_max_height")
  if (!is.null(max_height)) tag$attribs$style <- paste0(tag$attribs$style, ";max-height:", max_height, ";")
  prefix <- paste0(tag$attribs$id, "-")
  for (i in seq_along(tag$children)) {
    if (!inherits(tag$children[[i]], "html")) next
    text <- as.character(tag$children[[i]])
    pattern <- '(<th\\b[^>]*\\s+id=")([^"]+)("[^>]*>)'
    positions <- gregexpr(pattern, text, perl = TRUE)
    matches <- regmatches(text, positions)[[1]]
    if (!length(matches)) next
    ids <- character()
    replaced <- vapply(matches, function(match) {
      if (!grepl('class="[^"]*gt_', match)) return(match)
      groups <- regmatches(match, regexec(pattern, match, perl = TRUE))[[1]]
      id <- groups[[3]]
      ids[[id]] <<- paste0(prefix, id)
      paste0(groups[[2]], prefix, id, groups[[4]])
    }, character(1))
    regmatches(text, positions) <- list(replaced)
    references <- '([[:space:]](?:headers|aria-labelledby|aria-describedby)=")([^"]+)(")'
    positions <- gregexpr('<(?:td|th|table)\\b[^>]*>', text, perl = TRUE)
    matches <- regmatches(text, positions)[[1]]
    replaced <- vapply(matches, function(opening) {
      if (!grepl('class="[^"]*gt_', opening)) return(opening)
      attributes <- gregexpr(references, opening, perl = TRUE)
      values <- regmatches(opening, attributes)[[1]]
      adjusted <- vapply(values, function(value) {
        groups <- regmatches(value, regexec(references, value, perl = TRUE))[[1]]
        tokens <- strsplit(groups[[3]], "[[:space:]]+")[[1]]
        tokens <- ifelse(tokens %in% names(ids), ids[tokens], tokens)
        paste0(groups[[2]], paste(tokens, collapse = " "), groups[[4]])
      }, character(1))
      regmatches(opening, attributes) <- list(adjusted)
      opening
    }, character(1))
    regmatches(text, positions) <- list(replaced)
    tag$children[[i]] <- htmltools::HTML(text)
  }
  tag
}

.rsh_has_content <- function(value) {
  if (is.null(value)) return(FALSE)
  if (inherits(value, "shiny.tag")) return(TRUE)
  if (is.list(value)) return(any(vapply(value, .rsh_has_content, logical(1))))
  if (is.character(value)) return(any(nzchar(value)))
  length(value) > 0L
}
