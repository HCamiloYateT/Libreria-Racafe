\name{racafeTableNote}
\alias{racafeTableNote}
\title{Nota compacta externa para tablas y cajas}
\usage{
racafeTableNote(items = character(), title = NULL, font_size = 11,
  color = "#495057", swatches = NULL)
}
\arguments{
\item{items}{Textos o lista de componentes; cada elemento ocupa una línea.}
\item{title}{Título opcional.}
\item{font_size}{Tamaño positivo en píxeles.}
\item{color}{Color CSS del texto.}
\item{swatches}{Colores opcionales, uno por elemento, para convenciones visuales.}
}
\description{Crea una nota pequeña, alineada a la izquierda, fuera del contenedor de la tabla.}
\value{Componente HTML.}
\examples{
racafeTableNote(c("Sacos equivalentes de 70 kg.", "Solo meses cerrados."))
racafeTableNote(c("Anterior", "Mensual"), swatches = c("#F0FDF4", "#F8FAFC"))
}
