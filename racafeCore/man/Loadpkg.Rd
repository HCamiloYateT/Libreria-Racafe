% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{Loadpkg}
\alias{Loadpkg}
\title{Cargar paquetes instalando los faltantes}
\usage{
Loadpkg(...)
}
\arguments{
  \item{pkg}{Vector de caracteres con nombres de paquetes.}
}
\value{Vector logico nombrado con el resultado de `library()` por paquete.}
\description{
Verifica si los paquetes solicitados estan instalados; en caso
contrario, intenta instalarlos con dependencias y luego cargarlos.
}
