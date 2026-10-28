# Paneles y tarjetas ----

#' Panel corporativo sin altura ni icono obligatorios
#' @param title Título opcional.
#' @param ... Contenido del componente.
#' @param variant Apariencia: default, muted o highlighted.
#' @param padding Espaciado: compact, normal o comfortable.
#' @param min_height Altura mínima CSS opcional; NULL no impone ninguna.
#' @param bordered Mostrar borde.
#' @param footer Contenido inferior opcional.
#' @param header Contenido adicional del encabezado, después del título; NULL lo omite.
#' @return Etiqueta HTML con dependencia base; requiere racafeSharedAssets en la UI.
#' @export
racafePanel <- function(title = NULL, ..., variant = c("default", "muted", "highlighted"),
                         padding = c("normal", "compact", "comfortable"), min_height = NULL,
                         bordered = TRUE, footer = NULL, header = NULL) {
  variant <- match.arg(variant)
  padding <- match.arg(padding)
  .rf_flag(bordered, "bordered")
  .rf_attach(shiny::tags$section(
    class = paste("rf-panel", paste0("rf-panel--", variant), paste0("rf-padding--", padding),
                  if (bordered) "rf-panel--bordered"),
    style = if (!is.null(min_height)) paste0("min-height:", shiny::validateCssUnit(min_height), ";"),
    if (!is.null(title)) shiny::tags$h3(class = "rf-panel-title", title),
    if (!is.null(header)) shiny::tags$div(class = "rf-panel-header", header),
    shiny::tags$div(class = "rf-panel-body", ...),
    if (!is.null(footer)) shiny::tags$div(class = "rf-panel-footer", footer)))
}

#' Tarjeta informativa con interacción explícitamente optativa
#' @inheritParams racafePanel
#' @param icon Icono opcional: nombre Font Awesome o etiqueta shiny::icon.
#' @param clickable Activar clic y teclado como acción Shiny. FALSE no registra acciones.
#' @param id ID local, obligatorio solo si clickable es TRUE.
#' @param ns Función namespace del módulo.
#' @return Etiqueta de tarjeta; con clickable, `input$id` recibe un contador de activaciones.
#' @details La altura se adapta al contenido; no se estira para igualar tarjetas vecinas.
#'   `padding` controla la densidad sin CSS de aplicación. Requiere Compartido >= 1.1.0.
#'   En tarjetas interactivas no anide botones o enlaces; use footer externo para acciones adicionales.
#' @export
racafeCard <- function(title = NULL, ..., footer = NULL, icon = NULL,
                        variant = c("default", "muted", "highlighted"), clickable = FALSE, id = NULL, ns = NULL,
                        padding = c("normal", "compact", "comfortable"), header = NULL) {
  variant <- match.arg(variant)
  padding <- match.arg(padding)
  .rf_flag(clickable, "clickable")
  .rf_attach(shiny::tags$div(class = paste("rf-card", paste0("rf-card--", variant), paste0("rf-padding--", padding)),
    id = if (!is.null(id)) .rf_id(id, ns), role = if (clickable) "button",
    tabindex = if (clickable) "0", `data-rf-action` = if (clickable) .rf_id(id, ns),
    if (!is.null(title) || !is.null(icon)) shiny::tags$div(class = "rf-card-heading", .rf_icon(icon),
      if (!is.null(title)) shiny::tags$h4(class = "rf-card-title", title)),
    if (!is.null(header)) shiny::tags$div(class = "rf-card-header", header),
    shiny::tags$div(class = "rf-card-body", ...),
    if (!is.null(footer)) shiny::tags$div(class = "rf-card-footer", footer)))
}

#' Indicador numérico o textual
#' @param value Valor del indicador; se muestra sin aplicar reglas de negocio.
#' @param label Etiqueta descriptiva.
#' @param detail Texto o contenido auxiliar opcional.
#' @inheritParams racafeCard
#' @details label es el título y detail es la nota. padding = "compact" reduce espaciado
#'   y tamaño del valor sin añadir CSS a la aplicación. Icono y nota son opcionales.
#' @return Tarjeta informativa sin clic predeterminado.
#' @export
racafeMetric <- function(value, label, detail = NULL, icon = NULL,
                          variant = c("default", "muted", "highlighted"),
                          padding = c("normal", "compact", "comfortable")) {
  tag <- racafeCard(title = NULL, icon = icon, variant = match.arg(variant), padding = match.arg(padding),
    shiny::tags$span(class = "rf-metric-label", label),
    shiny::tags$strong(class = "rf-metric-value", value),
    if (!is.null(detail)) shiny::tags$span(class = "rf-metric-detail", detail))
  shiny::tagAppendAttributes(tag, class = "rf-metric")
}

#' Cuadrícula adaptable de indicadores
#' @param ... Componentes que se distribuyen en la cuadrícula.
#' @param columns Número de columnas de escritorio, de 1 a 4; en móvil se reduce automáticamente.
#' @return Contenedor HTML de cuadrícula.
#' @export
racafeMetricGrid <- function(..., columns = 3L) {
  .rf_attach(shiny::tags$div(class = paste("rf-grid rf-metric-grid", paste0("rf-grid--", .rf_columns(columns))), ...))
}

#' Cuadrícula adaptable de datos informativos
#' @inheritParams racafeMetricGrid
#' @return Contenedor HTML para pares etiqueta-valor.
#' @export
racafeInfoGrid <- function(..., columns = 2L) {
  .rf_attach(shiny::tags$div(class = paste("rf-grid rf-info-grid", paste0("rf-grid--", .rf_columns(columns))), ...))
}

#' Dato informativo de etiqueta y valor
#' @param label Etiqueta del dato.
#' @param value Valor o contenido HTML de Shiny.
#' @param empty Texto para valores NULL, vacíos o NA escalares.
#' @return Etiqueta HTML semántica con el par etiqueta-valor.
#' @export
racafeInfoItem <- function(label, value = NULL, empty = "Sin informaci\u00f3n") {
  if (is.null(value) || length(value) == 0L ||
      (is.atomic(value) && length(value) == 1L && (is.na(value) || identical(value, "")))) value <- empty
  .rf_attach(shiny::tags$dl(class = "rf-info-item", shiny::tags$dt(label), shiny::tags$dd(value)))
}
