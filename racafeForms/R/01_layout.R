#' Dependencia CSS de racafeForms
#' @export
racafeFormsDependencies <- function() {
  htmltools::htmlDependency("racafeForms", as.character(utils::packageVersion("racafeForms")),
                            src = c(file = "www"),
                            stylesheet = "racafe-forms.css", package = "racafeForms")
}

#' Contenedor visual de formulario
#' @param ... Contenido Shiny.
#' @param size Densidad: compact, normal o comfortable.
#' @param bordered Mostrar fondo y borde corporativo.
#' @export
racafeForm <- function(..., size = c("compact", "normal", "comfortable"), bordered = TRUE) {
  size <- match.arg(size)
  shiny::tagList(racafeFormsDependencies(),
    shiny::tags$div(class = paste("rf-form", paste0("rf-form--", size),
                                 if (bordered) "rf-form--bordered"), ...))
}

#' Grupo con divisoria opcional
#' @param ... Campos o texto.
#' @param title Titulo opcional del grupo.
#' @param divider Separador inferior.
#' @export
racafeFormGroup <- function(..., title = NULL, divider = TRUE) {
  shiny::tags$section(class = paste("rf-form-group", if (divider) "rf-form-group--divider"),
                      if (!is.null(title)) shiny::tags$h4(class = "rf-group-title", title), ...)
}

#' Fila flexible y adaptable de campos
#' @param ... Componentes a distribuir horizontalmente.
#' @param columns Numero de columnas en escritorio (1 a 4).
#' @export
racafeFormRow <- function(..., columns = 2L) {
  stopifnot(length(columns) == 1L, columns %in% 1:4)
  shiny::tags$div(class = paste("rf-row", paste0("rf-row--", columns)),
                  lapply(list(...), function(x) shiny::tags$div(class = "rf-col", x)))
}

#' Pie de formulario con acciones compactas
#' @param ... Botones.
#' @param help Texto opcional a la izquierda.
#' @param align left, center, right.
#' @param divider Mostrar borde superior.
#' @param sticky Fijar al pie dentro del contenedor scrollable.
#' @export
racafeFormFooter <- function(..., help = NULL, align = c("right", "left", "center"),
                             divider = TRUE, sticky = FALSE) {
  align <- match.arg(align)
  shiny::tags$div(class = paste("rf-footer", paste0("rf-footer--", align),
      if (divider) "rf-footer--divider", if (sticky) "rf-footer--sticky"),
    if (!is.null(help)) shiny::tags$span(class = "rf-footer-help", help),
    shiny::tags$div(class = "rf-footer-actions", ...))
}

#' Aviso de formulario no modal
#' @param text Mensaje.
#' @param type info, success, warning, error, muted.
#' @export
racafeFormMessage <- function(text, type = c("info", "success", "warning", "error", "muted")) {
  type <- match.arg(type)
  shiny::tags$p(class = paste("rf-message", paste0("rf-message--", type)), text)
}
