# Estados de salida y renderizadores con evaluación diferida ----
#' racafeEvaluateOutput: componente reutilizable del ecosistema Racafé
#' @export
racafeEvaluateOutput <- function(calcular) {
  tryCatch(calcular(), error = function(error) {
    if (inherits(error, "validation") && nzchar(conditionMessage(error))) stop(error)
    if (inherits(error, "shiny.silent.error")) {
      shiny::validate(shiny::need(FALSE, "No hay datos para la selección actual. Revise los filtros y los campos."))
    }
    message("[Cálculo] ", conditionMessage(error))
    shiny::validate(shiny::need(FALSE, "No se pudo completar el cálculo. Revise los datos e intente actualizar."))
  })
}

#' racafeRenderUI: componente reutilizable del ecosistema Racafé
#' @export
racafeRenderUI <- function(expr, env = parent.frame(), quoted = FALSE) {
  funcion <- shiny::exprToFunction(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
  shiny::renderUI(racafeEvaluateOutput(funcion))
}

#' racafeRenderPlot: componente reutilizable del ecosistema Racafé
#' @export
racafeRenderPlot <- function(expr, ..., env = parent.frame(), quoted = FALSE) {
  funcion <- shiny::exprToFunction(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
  shiny::renderPlot(racafeEvaluateOutput(funcion), ...)
}

#' racafeRenderPlotly: componente reutilizable del ecosistema Racafé
#' @export
racafeRenderPlotly <- function(expr, env = parent.frame(), quoted = FALSE, transform = NULL) {
  funcion <- shiny::exprToFunction(if (quoted) expr else substitute(expr), env = env, quoted = TRUE)
  plotly::renderPlotly({
    figura <- racafeEvaluateOutput(funcion)
    shiny::validate(shiny::need(!is.null(figura), "No hay datos para los filtros seleccionados."))
    figura <- plotly::plotly_build(figura)
    if (!is.null(transform)) figura <- transform(figura)
    tiene_datos <- any(vapply(figura$x$data, function(traza) {
      any(vapply(traza[intersect(c("x", "y", "z", "labels", "values", "value", "text", "lat", "lon"),
        names(traza))], length, integer(1)) > 0L) || length(traza$link$value) > 0L
    }, logical(1)))
    shiny::validate(shiny::need(tiene_datos || length(figura$x$layout$annotations),
      "No hay datos para los filtros seleccionados."))
    racafeGraph::aplicar_tipografia_plotly(figura)
  })
}

