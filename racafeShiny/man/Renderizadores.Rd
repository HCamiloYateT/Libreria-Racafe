\name{Renderizadores}
\alias{racafeEvaluateOutput}
\alias{racafeRenderUI}
\alias{racafeRenderPlot}
\alias{racafeRenderPlotly}
\title{Salidas con evaluación diferida y estados explícitos}
\description{Conserva validaciones Shiny, distingue selección sin datos de error y deja el mensaje técnico en el log. Los renderizadores mantienen el entorno del consumidor. Plotly admite una transformación visual optativa.}
\usage{
racafeEvaluateOutput(calcular)
racafeRenderUI(expr, env = parent.frame(), quoted = FALSE)
racafeRenderPlot(expr, ..., env = parent.frame(), quoted = FALSE)
racafeRenderPlotly(expr, env = parent.frame(), quoted = FALSE, transform = NULL)
}
\arguments{
\item{calcular}{Función sin argumentos que calcula la salida.}
\item{expr}{Expresión de salida evaluada de forma diferida en su entorno original.}
\item{env}{Entorno de evaluación de la expresión.}
\item{quoted}{Indica si expr ya es una expresión capturada.}
\item{...}{Contenido Shiny, argumentos del renderizador o del servidor de tabla, según la función.}
\item{transform}{Argumento de configuración del componente.}
}
\value{Objeto de presentación, cálculo o contrato reactivo descrito en los detalles de la función.}
