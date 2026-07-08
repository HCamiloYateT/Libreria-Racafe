% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{SidebarItemWrap}
\alias{SidebarItemWrap}
\title{Envolver un `bs4SidebarMenuItem` en un contenedor con `id`}
\usage{
SidebarItemWrap(...)
}
\arguments{
  \item{label}{Etiqueta visible del item.}
  \item{tabName}{Nombre de la pestana asociada.}
  \item{icon}{Icono del item (por ejemplo `shiny::icon("home")`).}
}
\value{Tag HTML (`div`) con el menu item dentro.}
\description{
Crea un `div` con id `wrap_<tabName>` que contiene un
`bs4SidebarMenuItem`, util para manipular estilos/visibilidad desde JS o CSS.
}
\examples{
\dontrun{
SidebarItemWrap("Inicio", tabName = "home", icon = shiny::icon("house"))
}
}
