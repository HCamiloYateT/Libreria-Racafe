# Navegación y selección de periodos corporativos ----
#' Selector de año y mes
#' @inheritParams racafeInputDate
#' @return Control Date con vista mensual. value, min y max se normalizan al primer día del mes.
#' @export
racafeInputMonth <- function(id, label = "Año y mes", value = Sys.Date(), min = NULL, max = NULL,
                              ns = NULL, width = "100%", disabled = FALSE, required = FALSE,
                              help = NULL, error = NULL) {
  mes <- function(x) if (is.null(x)) NULL else as.Date(format(as.Date(x), "%Y-%m-01"))
  racafeInputDate(id, label, value = mes(value), view = "month", min = mes(min), max = mes(max),
    ns = ns, width = width, disabled = disabled, required = required, help = help, error = error)
}
#' Contenido desplegable corporativo
#' @param title Texto o componente del encabezado.
#' @param ... Contenido Shiny, conservando los IDs y namespaces existentes.
#' @param open TRUE muestra inicialmente el contenido.
#' @return Panel details accesible con summary nativo, sin listeners globales.
#' @export
racafeDisclosure <- function(title, ..., open = FALSE) {
  .rf_attach(shiny::tags$details(class = "rf-disclosure", open = if (isTRUE(open)) NA else NULL,
    shiny::tags$summary(class = "rf-disclosure-title", title),
    shiny::tags$div(class = "rf-disclosure-body", ...),
    shiny::tags$style(".rf-disclosure{border:1px solid #dee2e6;border-radius:6px;margin-bottom:14px;}
      .rf-disclosure-title{padding:12px 16px;color:#343a40;font-weight:600;cursor:pointer;}
      .rf-disclosure-body{padding:0 16px 14px;}")))
}
#' Pestañas con navegación corporativa
#' @param id,selected,type Argumentos de shiny::tabsetPanel; id puede ser NULL.
#' @param ns Namespace opcional del módulo.
#' @param ... Paneles de shiny::tabPanel, conservando sus valores y contenido.
#' @return Contenedor de pestañas con colores corporativos y eventos Shiny nativos.
#' @export
racafeTabs <- function(..., id = NULL, selected = NULL, type = c("tabs", "pills"), ns = NULL) {
  .rf_attach(shiny::tags$div(class = "rf-tabs",
    shiny::tabsetPanel(..., id = if (!is.null(id)) .rf_id(id, ns), selected = selected, type = match.arg(type)),
    shiny::tags$style(".rf-tabs>.tabbable>.nav .nav-link{color:#495057;}
      .rf-tabs>.tabbable>.nav .nav-link.active{color:#DA291C;border-bottom:2px solid #DA291C;}
      .rf-tabs>.tabbable>.tab-content{padding-top:14px;}")))
}
#' Actualizar un selector mensual
#' @param session Sesión Shiny del módulo.
#' @param id ID local del control.
#' @param value,min,max Fechas normalizadas al primer día de mes; NULL no establece fecha.
#' @return Actualización del control mensual corporativo.
#' @export
racafeUpdateMonth <- function(session, id, value = NULL, min = NULL, max = NULL) {
  mes <- function(x) if (is.null(x)) NULL else as.Date(format(as.Date(x), "%Y-%m-01"))
  racafeUpdateDate(session, id, value = mes(value), options = list(minDate = mes(min), maxDate = mes(max)))
}
