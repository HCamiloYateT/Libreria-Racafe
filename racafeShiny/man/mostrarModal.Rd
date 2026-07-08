% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{mostrarModal}
\alias{mostrarModal}
\title{Mostrar modal Shiny con tamanos corporativos Racafe}
\usage{
mostrarModal(...)
}
\arguments{
  \item{...}{Contenido del modal, pasado a `shiny::modalDialog()`.}
  \item{titulo}{Titulo del modal. `NULL` por defecto.}
  \item{footer}{Pie del modal. Por defecto `shiny::modalButton("Cerrar")`.}
  \item{tamano}{Clase CSS para el tamano del modal. Debe ser una de `"ventana"`, `"subventana1"`, `"subventana2"`, `"subventana3"`, `"subventana4"`, `"subventana5"` o `"aviso"`. Si no es valida, se emite una advertencia y se usa `"subventana3"`.}
  \item{session}{Sesion Shiny donde se muestra el modal. Por defecto usa `shiny::getDefaultReactiveDomain()`. }
}
\value{Resultado de `shiny::showModal()`.}
\description{
Envuelve `shiny::modalDialog()` y agrega una clase CSS de tamano compatible
con las clases definidas en `Styles/style.css` del repositorio Compartido.
}
