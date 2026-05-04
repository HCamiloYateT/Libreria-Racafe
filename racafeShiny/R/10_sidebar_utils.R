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
