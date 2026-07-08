% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{LeerExcelDesdeOneDrive}
\alias{LeerExcelDesdeOneDrive}
\title{Leer un Excel desde OneDrive con readxl}
\usage{
LeerExcelDesdeOneDrive(...)
}
\arguments{
  \item{archivo_id}{ID del archivo Excel.}
  \item{usuario}{UPN o alias del usuario.}
  \item{...}{Argumentos adicionales pasados a `readxl::read_excel`.}
}
\value{`tibble` con el contenido del archivo.}
\description{
Leer un Excel desde OneDrive con readxl
}
\examples{
\dontrun{
df <- LeerExcelDesdeOneDrive("ABC123", "juan.perez", sheet = "Datos", skip = 1)
}
}
