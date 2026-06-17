# ============================================================
# Seccion 11 — Utilidades para modales Shiny
# ============================================================

# Clases de tamano soportadas por Styles/style.css de Compartido.
.modal_tamanos_validos <- c(
  "ventana", "subventana1", "subventana2", "subventana3",
  "subventana4", "subventana5", "aviso"
)

#' Mostrar modal Shiny con tamanos corporativos Racafe
#'
#' Envuelve `shiny::modalDialog()` y agrega una clase CSS de tamano compatible
#' con las clases definidas en `Styles/style.css` del repositorio Compartido.
#'
#' @param ... Contenido del modal, pasado a `shiny::modalDialog()`.
#' @param titulo Titulo del modal. `NULL` por defecto.
#' @param footer Pie del modal. Por defecto `shiny::modalButton("Cerrar")`.
#' @param tamano Clase CSS para el tamano del modal. Debe ser una de
#'   `"ventana"`, `"subventana1"`, `"subventana2"`, `"subventana3"`,
#'   `"subventana4"`, `"subventana5"` o `"aviso"`. Si no es valida,
#'   se emite una advertencia y se usa `"subventana3"`.
#' @param session Sesion Shiny donde se muestra el modal. Por defecto usa
#'   `shiny::getDefaultReactiveDomain()`.
#'
#' @return Resultado de `shiny::showModal()`.
#' @export
mostrarModal <- function(...,
                         titulo  = NULL,
                         footer  = shiny::modalButton("Cerrar"),
                         tamano  = "subventana3",
                         session = shiny::getDefaultReactiveDomain()) {
  if (!is.character(tamano) || length(tamano) != 1 || is.na(tamano) ||
      !nzchar(tamano) || !tamano %in% .modal_tamanos_validos) {
    warning(sprintf(
      "Clase '%s' no reconocida. Usando 'subventana3'.",
      paste(tamano, collapse = ", ")
    ), call. = FALSE)
    tamano <- "subventana3"
  }

  dlg <- htmltools::tagAppendAttributes(
    shiny::modalDialog(..., title = titulo, footer = footer, easyClose = FALSE),
    class = tamano
  )

  shiny::showModal(dlg, session = session)
}
