#' Dependencias visuales de racafeForms
#' @return Objeto `htmltools::htmlDependency` con los estilos y comportamientos del paquete.
#' @export
racafeFormsDependencies <- function() {
  htmltools::htmlDependency("racafeForms", as.character(utils::packageVersion("racafeForms")),
                            src = c(file = "www"),
                            stylesheet = "racafe-forms.css", script = "racafe-forms.js", package = "racafeForms")
}

#' Contenedor visual de formulario
#' @param ... Contenido Shiny.
#' @param size Densidad: compact, normal o comfortable.
#' @param bordered Mostrar fondo y borde corporativo.
#' @param id ID local opcional, necesario para racafeResetForm.
#' @param ns Función namespace del módulo.
#' @param unstyled Omitir el contenedor visual con borde y fondo, conservando estilos de los campos.
#' @return Lista de etiquetas Shiny con el contenedor del formulario y su dependencia CSS.
#' @export
racafeForm <- function(..., size = c("compact", "normal", "comfortable"), bordered = TRUE,
                         id = NULL, ns = NULL, unstyled = FALSE) {
  size <- match.arg(size)
  .rf_flag(unstyled, "unstyled")
  .rf_flag(bordered, "bordered")
  shiny::tagList(racafeFormsDependencies(),
    shiny::tags$div(class = paste("rf-form", paste0("rf-form--", size),
                                 if (bordered && !unstyled) "rf-form--bordered"),
                    id = if (!is.null(id)) .rf_id(id, ns), ...))
}

#' Grupo con divisoria opcional
#' @param ... Campos o texto.
#' @param title Titulo opcional del grupo.
#' @param divider Separador inferior.
#' @return Etiqueta HTML `section` con los campos y el título opcional.
#' @export
racafeFormGroup <- function(..., title = NULL, divider = TRUE) {
  shiny::tags$section(class = paste("rf-form-group", if (divider) "rf-form-group--divider"),
                      if (!is.null(title)) shiny::tags$h4(class = "rf-group-title", title), ...)
}

#' Fila flexible y adaptable de campos
#' @param ... Componentes a distribuir horizontalmente.
#' @param columns Numero de columnas en escritorio (1 a 4).
#' @return Etiqueta HTML `div` que distribuye los campos en columnas adaptables.
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
#' @param left,center,right Grupos opcionales para racafeActionBar; NULL conserva el diseño anterior.
#' @param wrap Permitir que los grupos nuevos pasen a otra línea.
#' @return Etiqueta HTML `div` con las acciones y el texto de ayuda opcional.
#' @export
racafeFormFooter <- function(..., help = NULL, align = c("right", "left", "center"),
                             divider = TRUE, sticky = FALSE, left = NULL, center = NULL, right = NULL, wrap = TRUE) {
  align <- match.arg(align)
  if (!is.null(left) || !is.null(center) || !is.null(right)) {
    return(shiny::tags$div(class = paste("rf-footer", if (divider) "rf-footer--divider",
                                        if (sticky) "rf-footer--sticky"),
      racafeActionBar(shiny::tagList(help, left), center, shiny::tagList(right, ...), wrap = wrap)))
  }
  shiny::tags$div(class = paste("rf-footer", paste0("rf-footer--", align),
      if (divider) "rf-footer--divider", if (sticky) "rf-footer--sticky"),
    if (!is.null(help)) shiny::tags$span(class = "rf-footer-help", help),
    shiny::tags$div(class = "rf-footer-actions", ...))
}

#' Aviso de formulario no modal
#' @param text Mensaje.
#' @param type info, success, warning, error, muted, loading o empty.
#' @param details Detalles técnicos opcionales, explícitos y escapados; nunca se añaden desde errores automáticamente.
#' @return Etiqueta HTML accesible con el mensaje, estado y detalles opcionales.
#' @export
racafeFormMessage <- function(text, type = c("info", "success", "warning", "error", "muted", "loading", "empty"),
                                details = NULL) {
  if (identical(type, "e")) type <- "error"
  type <- match.arg(type)
  if (!is.null(details)) details <- .rf_scalar(unclass(details), "details", empty = TRUE)
  container <- if (is.null(details)) shiny::tags$p else shiny::tags$div
  .rf_attach(container(class = paste("rf-message", paste0("rf-message--", type)),
    role = if (type %in% c("error", "warning")) "alert" else "status",
    `aria-live` = if (type == "error") "assertive" else "polite",
    `aria-busy` = if (type == "loading") "true", text,
    if (!is.null(details)) shiny::tags$details(class = "rf-message-details",
      shiny::tags$summary("Detalles t\u00e9cnicos"), shiny::tags$pre(details))))
}
