# Etiquetas semánticas ----

#' Badge corporativo de estado, independiente de reglas de negocio
#' @param value Texto o valor escalar no NA; se escapa como texto, no como HTML.
#' @param variant Variante semántica: success, warning, danger, info o neutral.
#' @param size Tamaño: xs, sm o md. sm es el predeterminado compacto.
#' @return Etiqueta span informativa con dependencia base; requiere Compartido >= 1.1.0.
#' @details Puede usarse en tarjetas, modales y celdas de reactable. Mantenga una
#'   etiqueta descriptiva: el significado no depende solo del color. No anuncia
#'   cada celda como una región viva ni registra acciones o eventos Shiny.
#' @examples
#' racafeStatusBadge("Disponible", variant = "success")
#' racafeStatusBadge("Pendiente", variant = "warning", size = "xs")
#' @export
racafeStatusBadge <- function(value, variant = "neutral", size = "sm") {
  variant <- match.arg(variant, c("success", "warning", "danger", "info", "neutral"))
  size <- match.arg(size, c("xs", "sm", "md"))
  if (!is.atomic(value) || length(value) != 1L || is.na(value)) {
    stop("value debe ser un valor escalar no NA.", call. = FALSE)
  }
  value <- as.character(value)
  .rf_attach(shiny::tags$span(class = paste("rf-status-badge", paste0("rf-status-badge--", variant),
                                          paste0("rf-status-badge--", size)), value))
}
