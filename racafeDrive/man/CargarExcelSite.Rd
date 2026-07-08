% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{CargarExcelSite}
\alias{CargarExcelSite}
\title{Descargar y leer un Excel desde un drive de SharePoint por ID}
\usage{
CargarExcelSite(...)
}
\arguments{
  \item{drive_id}{ID del drive de SharePoint.}
  \item{item_id}{ID del item (archivo) dentro del drive.}
  \item{hoja}{Nombre o indice de hoja. `NULL` lee la primera.}
  \item{...}{Argumentos adicionales para `readxl::read_excel`.}
}
\value{`tibble` con el contenido del archivo.}
\description{
Descargar y leer un Excel desde un drive de SharePoint por ID
}
