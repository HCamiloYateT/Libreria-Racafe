# Componentes corporativos de detalle ----
.rsh_ui_dependency <- function() {
  htmltools::htmlDependency("racafe-shiny-ui", "1.3.0", src = c(file = system.file("ui", package = "racafeShiny")),
                            stylesheet = "components.css", script = "components.js")
}
.rsh_ui_text <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop(name, " debe ser un texto no vacío.", call. = FALSE)
  }
  as.vector(value, mode = "character")
}
.rsh_ui_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) stop(name, " debe ser lógico.", call. = FALSE)
}

#' Sección colapsable corporativa con HTML nativo
#' @param id ID del control; en módulos use ns(id).
#' @param title Título de texto plano.
#' @param open Estado inicial, FALSE cerrado.
#' @param ... Cualquier contenido Shiny, incluidos outputs, formularios y módulos.
#' @param icon Icono Font Awesome opcional (nombre o etiqueta).
#' @param color Color del triángulo, hexadecimal o nombre reconocido por R.
#' @param class Clases CSS adicionales del contenedor.
#' @return Etiqueta details. input$id contiene TRUE/FALSE al estar enlazada a Shiny.
#' @details Usa details/summary nativos y teclado del navegador. El binding propio solo
#'   sincroniza el estado con Shiny y permite actualizaciones; no añade librerías JS.
#'   No recree el componente para cambiar su estado: use racafeUpdateCollapsible.
#' @examples
#' racafeCollapsible("detalle", "Detalle de pedidos", open = TRUE, shiny::p("Contenido"))
#' @export
racafeCollapsible <- function(id, title, open = FALSE, ..., icon = NULL, color = "#DA291C", class = NULL) {
  id <- .rsh_ui_text(id, "id")
  title <- .rsh_ui_text(title, "title")
  .rsh_ui_flag(open, "open")
  color <- .rsh_ui_text(color, "color")
  rgba <- tryCatch(grDevices::col2rgb(color, alpha = TRUE),
    error = function(error) stop("color no es válido.", call. = FALSE))
  color <- grDevices::rgb(rgba[1, 1], rgba[2, 1], rgba[3, 1], alpha = rgba[4, 1], maxColorValue = 255)
  if (!is.null(class)) class <- .rsh_ui_text(class, "class")
  if (is.character(icon)) icon <- shiny::icon(.rsh_ui_text(icon, "icon"))
  if (!is.null(icon) && !inherits(icon, "shiny.tag")) stop("icon debe ser nombre o etiqueta.", call. = FALSE)
  tag <- shiny::tags$details(id = id, class = paste(c("rsh-collapsible", class), collapse = " "),
    open = if (open) "open", style = paste0("--rsh-collapse-color:", color, ";"),
    shiny::tags$summary(`aria-controls` = paste0(id, "_content"), `aria-expanded` = tolower(as.character(open)),
      icon, shiny::tags$span(title)),
    shiny::tags$div(id = paste0(id, "_content"), class = "rsh-collapsible-content", ...))
  htmltools::attachDependencies(tag, .rsh_ui_dependency())
}

#' Abrir o cerrar una sección colapsable desde el servidor
#' @param session Sesión Shiny, incluida una sesión de módulo.
#' @param id ID local, o ID que ya contiene el namespace de la sesión.
#' @param open TRUE para abrir, FALSE para cerrar.
#' @return Resultado de enviar el mensaje al binding nativo del componente.
#' @export
racafeUpdateCollapsible <- function(session = shiny::getDefaultReactiveDomain(), id, open) {
  if (is.null(session)) stop("Se requiere una sesión Shiny activa.", call. = FALSE)
  id <- .rsh_ui_text(id, "id")
  .rsh_ui_flag(open, "open")
  prefix <- session$ns("")
  if (nzchar(prefix) && startsWith(id, prefix)) id <- substring(id, nchar(prefix) + 1L)
  session$sendInputMessage(id, list(open = open))
}

#' Pie corporativo de modal reutilizando racafeForms
#' @param cerrar Incluir un botón de cierre nativo sin evento de guardado.
#' @param acciones Botones personalizados, por ejemplo racafeForms::racafeButtonSave().
#' @param ... Acciones adicionales, conservadas sin modificar sus bindings ni IDs.
#' @param texto_cerrar Etiqueta del botón nativo; puede ser Cerrar o Cancelar.
#' @return Contenedor para footer de shiny::modalDialog. Requiere racafeForms instalado.
#' @details Delega la estructura a racafeForms::racafeModalFooter. Los botones propios
#'   se aplican solo dentro del pie corporativo. Para Cancelar/Guardar con eventos,
#'   use cerrar FALSE, botones con IDs y removeModal en los observers de la aplicación.
#' @examples
#' \dontrun{
#' shiny::modalDialog("Contenido", footer = racafeModalFooter(acciones =
#'   racafeForms::racafeButtonSave("guardar", "Guardar")))
#' }
#' @export
racafeModalFooter <- function(cerrar = TRUE, acciones = NULL, ..., texto_cerrar = "Cerrar") {
  .check_pkg("racafeForms", "racafeModalFooter")
  .rsh_ui_flag(cerrar, "cerrar")
  close <- if (cerrar) shiny::tagAppendAttributes(shiny::modalButton(.rsh_ui_text(texto_cerrar, "texto_cerrar")),
    class = "rf-btn rf-btn--secondary rf-btn--sm")
  footer <- racafeForms::racafeModalFooter(right = shiny::tagList(close, acciones, ...))
  htmltools::attachDependencies(shiny::tags$div(class = "rsh-modal-footer", footer), .rsh_ui_dependency())
}
