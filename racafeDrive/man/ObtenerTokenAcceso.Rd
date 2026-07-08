% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{ObtenerTokenAcceso}
\alias{ObtenerTokenAcceso}
\title{Obtener token de acceso para Microsoft Graph}
\usage{
ObtenerTokenAcceso(...)
}
\arguments{
  \item{force}{Logico. Si `TRUE`, fuerza renovacion aunque no haya expirado.}
}
\value{Cadena con el access token.}
\description{
Implementa cache en memoria con verificacion de expiracion.
El token se renueva automaticamente al expirar.

Variables de entorno requeridas:
- `MS_TENANT_ID`: tenant ID de Azure AD
- `MS_CLIENT_ID`: client ID de la aplicacion Azure AD
- `MS_CLIENT_SECRET`: client secret
}
