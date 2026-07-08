% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ConsultaSistema}
\alias{ConsultaSistema}
\title{Ejecutar consulta en SQL Server con credenciales explicitas}
\usage{
ConsultaSistema(...)
}
\arguments{
  \item{bd}{Nombre de la base de datos.}
  \item{query}{Consulta SQL.}
  \item{uid}{Usuario. Por defecto `Sys.getenv("SYS_UID")`.}
  \item{pwd}{Contrasena. Por defecto `Sys.getenv("SYS_PWD")`.}
  \item{server}{IP del servidor.}
  \item{port}{Puerto TCP.}
}
\value{`data.frame` con los nombres de columnas originales.}
\description{
Funcion de compatibilidad para conectarse a servidores alternos.
Preferir `Consulta()` con variables de entorno para nuevos desarrollos.
}
