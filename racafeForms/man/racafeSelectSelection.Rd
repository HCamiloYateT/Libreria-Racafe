\name{racafeSelectSelection}
\alias{racafeSelectSelection}
\title{Conservar selección múltiple al actualizar opciones}
\description{Devuelve las opciones iniciales, conserva selección parcial o vacía y amplía cuando antes estaban todas seleccionadas; el estado es privado de la sesión y del namespace.}
\usage{
racafeSelectSelection(input, session, id, choices)
}
\arguments{
\item{input}{Lista reactiva de inputs de la sesión.}
\item{session}{Sesión Shiny propietaria del control.}
\item{id}{ID local del componente o módulo, conservando el namespace de Shiny.}
\item{choices}{Opciones disponibles. Una selección vacía sigue siendo vacía al actualizar.}
}
\value{Objeto de presentación, cálculo o contrato reactivo descrito en los detalles de la función.}
