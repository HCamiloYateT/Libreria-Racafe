\name{OperacionesDatos}
\alias{SumaEstricta}
\alias{ValorOCero}
\alias{IdentificadorTexto}
\title{Operaciones que preservan faltantes e identificadores}
\description{SumaEstricta propaga NA y devuelve cero para un vector vacío. ValorOCero conserva el contrato escalar de NULL/NA a cero. IdentificadorTexto conserva texto sin separadores de miles.}
\usage{
SumaEstricta(x)
ValorOCero(x)
IdentificadorTexto(x)
}
\arguments{
\item{x}{Vector, tabla o figura de entrada, según la función. No se modifica el objeto original.}
}
\value{Vector o resultado descrito para cada función.}
