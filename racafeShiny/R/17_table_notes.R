# Notas externas de tablas ----

#' Nota compacta y externa para tablas y cajas
#' @param items Textos o lista de componentes; cada elemento ocupa una línea.
#' @param title Título opcional.
#' @param font_size Tamaño positivo en píxeles.
#' @param color Color del texto; por defecto gris oscuro Racafé.
#' @param swatches Colores opcionales, uno por elemento, para convenciones visuales.
#' @return Nota HTML alineada a la izquierda, sin contenedor de tabla ni scroll.
#' @export
racafeTableNote <- function(items = character(), title = NULL, font_size = 11, color = "#495057",
                            swatches = NULL) {
  if (!is.character(items) && !is.list(items)) stop("items debe ser texto o una lista.", call. = FALSE)
  if (!is.numeric(font_size) || length(font_size) != 1L || !is.finite(font_size) || font_size <= 0) {
    stop("font_size debe ser positivo.", call. = FALSE)
  }
  .color <- function(x) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) || grepl("[;{}<>]", x)) {
      stop("Color CSS inválido.", call. = FALSE)
    }
    x
  }
  color <- .color(color)
  if (!is.null(swatches)) {
    if (length(swatches) != length(items)) stop("Debe haber un color por elemento.", call. = FALSE)
    swatches <- vapply(swatches, .color, character(1))
  }
  shiny::tags$div(class = "rsh-table-note", style = paste0("font-size:", font_size, "px;color:", color,
    ";line-height:1.4;text-align:left;margin:4px 0 8px;overflow-wrap:anywhere;"),
    if (!is.null(title)) shiny::tags$strong(title),
    lapply(seq_along(items), function(i) if (!is.null(items[[i]])) {
      shiny::tags$div(style = "display:flex;gap:6px;align-items:baseline;",
        if (!is.null(swatches)) shiny::tags$span(`aria-hidden` = "true", style = paste0(
          "display:inline-block;flex:0 0 10px;width:10px;height:10px;border:1px solid #6C757D;background:",
          swatches[[i]], ";")), shiny::tags$span(items[[i]]))
    }))
}
