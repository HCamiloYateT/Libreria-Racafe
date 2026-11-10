\name{AsociacionCestas}
\alias{ConjuntosFrecuentes}
\alias{MetricasAsociacion}
\alias{ReglasAsociacion}
\alias{ValidarReglasAsociacion}
\title{Conjuntos frecuentes y reglas de asociación}
\description{Cálculo sobre pertenencias preparadas, sin filtrar clientes ni reconstruir pedidos. Conserva límites de candidatos, métricas y validación en cestas separadas.}
\usage{
ConjuntosFrecuentes(membresia, cestas, soporte = 0.03, minimo = 3L, longitud = 3L, limite = 50000L)
MetricasAsociacion(n, a, b, ab)
ReglasAsociacion(modelo, confianza = 0.5, lift = 1, estadistica = TRUE, limite = 20000L)
ValidarReglasAsociacion(reglas, membresia, cestas)
}
\arguments{
\item{membresia}{Tabla de pertenencias con columnas Cesta e Item; cada pertenencia debe ser única.}
\item{cestas}{Identificadores del universo de cestas de entrenamiento o validación.}
\item{soporte}{Participación mínima de cestas que contiene un conjunto.}
\item{minimo}{Número mínimo de cestas que respalda un conjunto.}
\item{longitud}{Longitud máxima de los conjuntos candidatos.}
\item{limite}{Límite de candidatos o reglas; se genera un error en vez de devolver resultados truncados.}
\item{n}{Número de cestas del universo.}
\item{a}{Número de cestas que contiene el antecedente.}
\item{b}{Número de cestas que contiene el consecuente.}
\item{ab}{Número de cestas que contiene ambos.}
\item{modelo}{Modelo ajustado o conjuntos frecuentes producidos por la función correspondiente.}
\item{confianza}{Confianza mínima de una regla.}
\item{lift}{Afinidad mínima de una regla.}
\item{estadistica}{Calcular Fisher y ajuste BH sobre la familia completa antes del filtrado.}
\item{reglas}{Tabla de reglas de asociación a evaluar sobre las cestas de validación.}
}
\value{Objeto de presentación, cálculo o contrato reactivo descrito en los detalles de la función.}
