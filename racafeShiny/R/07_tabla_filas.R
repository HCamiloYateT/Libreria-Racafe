# ============================================================
# Seccion 07 — Reglas de estilo por fila para tablas HTML
# ============================================================

#' Definir una regla de estilo por fila
#'
#' Crea un objeto de regla reutilizable para evaluar cada fila de una tabla.
#' La regla puede agregar estilo CSS (como lista nombrada) y clases CSS.
#'
#' @param cuando Funcion con firma `function(fila, indice, data)` que retorna
#'   `TRUE` o `FALSE` para cada fila.
#' @param estilo Lista nombrada de propiedades CSS a aplicar cuando la regla
#'   cumple. Ej.: `list(`background-color` = "#FFF7E6", `font-weight` = 600)`.
#' @param clase Vector de clases CSS a agregar cuando la regla cumple.
#' @param nombre Nombre opcional de la regla para trazabilidad.
#' @param columnas Vector opcional de columnas objetivo (metadato para
#'   adaptadores por renderer).
#' @return Objeto clase `regla_fila`.
#' @export
ReglaFila <- function(cuando,
                      estilo = list(),
                      clase = character(),
                      nombre = NULL,
                      columnas = NULL) {
  if (!is.function(cuando)) {
    rlang::abort("`cuando` debe ser una funcion con firma function(fila, indice, data).")
  }

  if (!is.list(estilo) || is.null(names(estilo)) || any(names(estilo) == "")) {
    rlang::abort("`estilo` debe ser una lista nombrada de propiedades CSS.")
  }

  clase <- as.character(clase)
  if (is.null(nombre)) {
    nombre <- sprintf("regla_%s", as.integer(stats::runif(1, min = 1, max = 1e9)))
  }

  structure(
    list(
      nombre = as.character(nombre),
      cuando = cuando,
      estilo = estilo,
      clase = unique(clase[nzchar(clase)]),
      columnas = if (is.null(columnas)) NULL else as.character(columnas)
    ),
    class = "regla_fila"
  )
}

#' Aplicar reglas de estilo fila a fila sobre una tabla
#'
#' Evalua un conjunto de `ReglaFila()` sobre cada fila y devuelve metadatos
#' agnosticos al renderer (reactable, DT, gt, kableExtra, etc.).
#'
#' @param data `data.frame` de entrada.
#' @param reglas Lista de objetos `regla_fila`.
#' @return Lista con `data`, `estilos_fila`, `clases_fila`,
#'   `reglas_aplicadas` y `columnas_objetivo`.
#' @export
AplicarReglasFila <- function(data, reglas = list()) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` debe ser un data.frame.")
  }

  if (!is.list(reglas)) {
    rlang::abort("`reglas` debe ser una lista de objetos creados con ReglaFila().")
  }

  if (length(reglas) == 0) {
    return(list(
      data = data,
      estilos_fila = replicate(nrow(data), list(), simplify = FALSE),
      clases_fila = rep("", nrow(data)),
      reglas_aplicadas = replicate(nrow(data), character(), simplify = FALSE),
      columnas_objetivo = replicate(nrow(data), character(), simplify = FALSE)
    ))
  }

  invalidas <- !vapply(reglas, inherits, logical(1), what = "regla_fila")
  if (any(invalidas)) {
    rlang::abort("Todos los elementos en `reglas` deben ser `regla_fila`.")
  }

  n <- nrow(data)
  estilos <- vector("list", n)
  clases <- vector("list", n)
  aplicadas <- vector("list", n)
  cols_target <- vector("list", n)

  for (i in seq_len(n)) {
    fila <- as.list(data[i, , drop = FALSE])
    estilos_i <- list()
    clases_i <- character()
    aplicadas_i <- character()
    cols_i <- character()

    for (regla in reglas) {
      cumple <- isTRUE(regla$cuando(fila, i, data))
      if (!cumple) {
        next
      }

      estilos_i <- utils::modifyList(estilos_i, regla$estilo)
      clases_i <- unique(c(clases_i, regla$clase))
      aplicadas_i <- c(aplicadas_i, regla$nombre)
      if (!is.null(regla$columnas)) {
        cols_i <- unique(c(cols_i, regla$columnas))
      }
    }

    estilos[[i]] <- estilos_i
    clases[[i]] <- clases_i
    aplicadas[[i]] <- aplicadas_i
    cols_target[[i]] <- cols_i
  }

  list(
    data = data,
    estilos_fila = estilos,
    clases_fila = vapply(clases, paste, collapse = " ", FUN.VALUE = character(1)),
    reglas_aplicadas = aplicadas,
    columnas_objetivo = cols_target
  )
}

#' Fachada de una sola llamada para reglas por fila
#'
#' Atajo para construir/aplicar reglas por fila en una llamada.
#'
#' @inheritParams AplicarReglasFila
#' @return Misma estructura que `AplicarReglasFila()`.
#' @export
TablaFilas <- function(data, reglas = list()) {
  AplicarReglasFila(data = data, reglas = reglas)
}
