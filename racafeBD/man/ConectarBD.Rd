% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ConectarBD}
\alias{ConectarBD}
\title{Establecer conexion a la base de datos corporativa}
\usage{
ConectarBD(...)
}
\arguments{
  \item{bd}{Nombre de la base de datos. Por defecto `Sys.getenv("DB_NAME")`.}
  \item{server}{IP o hostname del servidor. Por defecto `Sys.getenv("DB_SERVER")`.}
  \item{port}{Puerto TCP. Por defecto `1433`.}
}
\value{Objeto de conexion `DBIConnection`.}
\description{
Lee credenciales exclusivamente desde variables de entorno.
Nunca pasar credenciales como argumentos.
}
\examples{
\dontrun{
con <- ConectarBD()
DBI::dbDisconnect(con)
}
}
