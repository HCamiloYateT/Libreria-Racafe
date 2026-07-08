% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ListarDriveRecursivo}
\alias{ListarDriveRecursivo}
\title{Listar archivos en un drive de SharePoint/OneDrive con metadatos}
\usage{
ListarDriveRecursivo(...)
}
\arguments{
  \item{drive_id}{ID del drive.}
  \item{item_id}{ID del item raiz. Por defecto `"root"`.}
  \item{ruta}{Ruta acumulada (uso interno recursivo).}
  \item{fecha_desde}{Fecha minima de modificacion para filtrar. Opcional.}
}
\value{`tibble` con metadatos: nombre, id, ruta, fecha_modificacion, tamano.}
\description{
Listar archivos en un drive de SharePoint/OneDrive con metadatos
}
