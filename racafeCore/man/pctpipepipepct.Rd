% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{\%||\%}
\alias{\%||\%}
\title{Operador null-coalescing}
\usage{
a \%||\% b
}
\arguments{
  \item{a}{Valor principal.}
  \item{b}{Valor por defecto.}
}
\value{`a` cuando no es `NULL` ni tiene longitud cero; en caso contrario `b`.}
\description{
Retorna el primer valor cuando está disponible y usa el segundo como valor por defecto.
}
\examples{
NULL \%||\% "valor_por_defecto"
"" \%||\% "fallback"
}
