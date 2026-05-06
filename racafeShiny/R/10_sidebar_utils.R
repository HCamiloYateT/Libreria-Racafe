# ============================================================
# Seccion 10 — Utilidades de sidebar
# Helpers de UI para componentes bs4Dash
# ============================================================

#' Envolver un `bs4SidebarMenuItem` en un contenedor con `id`
#'
#' Crea un `div` con id `wrap_<tabName>` que contiene un
#' `bs4SidebarMenuItem`, util para manipular estilos/visibilidad desde JS o CSS.
#'
#' @param label Etiqueta visible del item.
#' @param tabName Nombre de la pestana asociada.
#' @param icon Icono del item (por ejemplo `shiny::icon("home")`).
#' @return Tag HTML (`div`) con el menu item dentro.
#' @export
#' @examples
#' \dontrun{
#' SidebarItemWrap("Inicio", tabName = "home", icon = shiny::icon("house"))
#' }
SidebarItemWrap <- function(label, tabName, icon) {
  .check_pkg("bs4Dash", "SidebarItemWrap")

  shiny::div(
    id = paste0("wrap_", tabName),
    bs4Dash::bs4SidebarMenuItem(label, tabName = tabName, icon = icon)
  )
}

#' Validar estados de color de AdminLTE/Bootstrap
#'
#' @param status Estado visual a validar.
#' @return `status` de forma invisible si es valido.
#' @keywords internal
.validate_status <- function(status) {
  status_validos <- c(
    "primary", "secondary", "success", "info", "warning", "danger",
    "light", "dark", "white", "gray", "gray-dark", "indigo",
    "lightblue", "navy", "purple", "fuchsia", "pink", "maroon",
    "orange", "lime", "teal", "olive"
  )

  if (!is.character(status) || length(status) != 1 || !nzchar(status) ||
      !status %in% status_validos) {
    stop(sprintf(
      "badgeStatus '%s' no es válido. Opciones: %s",
      paste(status, collapse = ", "), paste(status_validos, collapse = ", ")
    ), call. = FALSE)
  }

  invisible(status)
}


#' Menu desplegable para navbar con badge, encabezado y pie opcionales
#'
#' Construye un item de navbar compatible con AdminLTE/bs4Dash para mostrar
#' mensajes, notificaciones o tareas. Permite controlar el icono, el badge,
#' el encabezado, el enlace de pie y clases CSS adicionales del menu.
#'
#' @param ... Items HTML que se renderizan dentro del menu desplegable.
#' @param type Tipo de menu. Uno de `"messages"`, `"notifications"` o
#'   `"tasks"`. Si se define, resuelve icono y encabezado por defecto.
#' @param badgeStatus Estado visual del badge. Acepta estados Bootstrap y
#'   colores AdminLTE (`"primary"`, `"success"`, `"warning"`, etc.).
#' @param icon Icono HTML. Si es `NULL`, se infiere desde `type`.
#' @param headerText Texto del encabezado. Si es `NULL` y `type` existe, se
#'   genera con el conteo de items.
#' @param .list Lista opcional de items HTML a concatenar con `...`.
#' @param href URL del enlace de pie. Si es `NULL`, no se muestra pie.
#' @param footerText Texto del enlace de pie.
#' @param showBadge Logico. Muestra u oculta el badge.
#' @param showHeader Logico. Muestra u oculta el encabezado.
#' @param numItems Conteo a mostrar. Si es `NULL`, usa la cantidad de items.
#' @param menuClass Clases CSS adicionales para el contenedor del menu.
#' @return Tag HTML (`li`) con el menu desplegable.
#' @export
#' @examples
#' \dontrun{
#' dropdownMenuPlus(
#'   type = "notifications",
#'   badgeStatus = "warning",
#'   href = "https://racafe.com",
#'   shiny::tags$a(class = "dropdown-item", href = "#", "Nueva notificacion")
#' )
#' }
dropdownMenuPlus <- function(...,
                             type        = NULL,
                             badgeStatus = NULL,
                             icon        = NULL,
                             headerText  = NULL,
                             .list       = NULL,
                             href        = NULL,
                             footerText  = "Ver más",
                             showBadge   = TRUE,
                             showHeader  = TRUE,
                             numItems    = NULL,
                             menuClass   = NULL) {

  # Validacion de argumentos ----
  if (!is.null(type)) type <- match.arg(type, c("messages", "notifications", "tasks"))
  if (!is.null(badgeStatus)) .validate_status(badgeStatus)

  # Consolidacion de items y conteo ----
  items    <- c(list(...), .list)
  numItems <- numItems %||% length(items)

  # Resolucion de icono: explicito > type-based > generico ----
  icon <- icon %||% switch(
    type %||% "",
    messages      = shiny::icon("comments"),
    notifications = shiny::icon("bell"),
    tasks         = shiny::icon("list-check"),
    shiny::icon("bars")
  )

  # Resolucion de header: explicito > type-based > NULL ----
  if (is.null(headerText) && !is.null(type)) {
    headerText <- paste("Tienes", numItems, switch(type,
      messages      = "mensaje(s)",
      notifications = "notificación(es)",
      tasks         = "tarea(s)"
    ))
  }

  # Construccion de secciones condicionales ----
  badge <- if (!is.null(badgeStatus) && showBadge) {
    shiny::tags$span(class = paste0("badge badge-", badgeStatus, " navbar-badge"), numItems)
  }

  header_ui <- if (!is.null(headerText) && showHeader) {
    shiny::tagList(
      shiny::tags$span(class = "dropdown-item dropdown-header", headerText),
      shiny::tags$div(class = "dropdown-divider")
    )
  }

  footer_ui <- if (!is.null(href)) {
    shiny::tags$a(
      class = "dropdown-item dropdown-footer",
      href = href,
      target = "_blank",
      footerText
    )
  }

  # Construccion del componente dropdown ----
  menu_class <- paste(c("dropdown-menu dropdown-menu-lg", menuClass), collapse = " ")

  shiny::tags$li(
    class = "nav-item dropdown",
    shiny::tags$a(
      class = "nav-link",
      `data-toggle` = "dropdown",
      href = "#",
      `aria-expanded` = "false",
      icon,
      badge
    ),
    shiny::tags$div(class = menu_class, header_ui, items, footer_ui)
  )
}
