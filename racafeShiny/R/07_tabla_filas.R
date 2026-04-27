# ============================================================
# Seccion 07 — Reglas de estilo por fila para tablas HTML
# ============================================================

.valor_por_defecto <- function(x, default) {
  if (is.null(x)) default else x
}

.estilo_desde_regla <- function(regla) {
  estilo <- list()

  if (!is.null(regla$alineacion)) {
    estilo$`text-align` <- regla$alineacion
  }
  if (!is.null(regla$tamano_pct)) {
    estilo$`font-size` <- sprintf("%s%%", round(as.numeric(regla$tamano_pct) * 100))
  }
  if (isTRUE(regla$cursiva)) {
    estilo$`font-style` <- "italic"
  }
  if (!is.null(regla$color)) {
    estilo$color <- regla$color
  }

  style_txt <- paste(
    paste(names(estilo), unlist(estilo), sep = ":"),
    collapse = ";"
  )

  list(estilo = estilo, style_txt = style_txt)
}

.aplicar_estilo_html <- function(html, style_txt) {
  if (!nzchar(style_txt)) {
    return(html)
  }

  htmltools::HTML(sprintf('<span style="display:block;%s">%s</span>', style_txt, html))
}

#' Definir una regla de estilo por fila
#'
#' Crea una regla simple basada en el valor de una columna por fila.
#'
#' @param etiqueta Valor de la fila a comparar (ej. "CUMPLIMIENTO %").
#' @param fmt Formato registrado para `FormatearNumero()`.
#' @param negrita Logico. Aplicar negrita al valor.
#' @param cursiva Logico. Aplicar cursiva al valor.
#' @param color Color del texto.
#' @param alineacion Alineacion del contenido (`"left"`, `"center"`, `"right"`).
#' @param tamano_pct Escala relativa de fuente (1 = 100%).
#' @param meta Meta opcional para color condicional en `FormatearNumero()`.
#' @param prop Logico para evaluar `meta` como proporcion.
#' @param clase Clases CSS para la fila.
#' @param columnas Columnas objetivo para aplicar formato.
#' @return Objeto clase `regla_fila`.
#' @export
ReglaFila <- function(etiqueta,
                      fmt = NULL,
                      negrita = TRUE,
                      cursiva = FALSE,
                      color = "#000000",
                      alineacion = "left",
                      tamano_pct = 1,
                      meta = NA,
                      prop = TRUE,
                      clase = character(),
                      columnas = NULL) {
  if (missing(etiqueta) || !length(etiqueta) || !nzchar(as.character(etiqueta)[1])) {
    rlang::abort("`etiqueta` es obligatoria y debe ser texto no vacio.")
  }

  structure(
    list(
      etiqueta = as.character(etiqueta)[1],
      fmt = if (is.null(fmt)) NULL else as.character(fmt)[1],
      negrita = isTRUE(negrita),
      cursiva = isTRUE(cursiva),
      color = as.character(color)[1],
      alineacion = as.character(alineacion)[1],
      tamano_pct = as.numeric(tamano_pct)[1],
      meta = meta,
      prop = isTRUE(prop),
      clase = unique(as.character(clase[nzchar(clase)])),
      columnas = if (is.null(columnas)) NULL else as.character(columnas)
    ),
    class = "regla_fila"
  )
}

#' Aplicar reglas por fila con una sintaxis compacta
#'
#' @param data `data.frame` de entrada.
#' @param col Columna usada para identificar la fila objetivo de cada regla.
#' @param reglas Lista de reglas. Cada regla puede ser `ReglaFila(...)` o
#'   una lista tipo `list("Etiqueta", fmt = "coma", cursiva = TRUE, meta = 1)`.
#' @return Lista con `data`, `estilos_fila`, `clases_fila`,
#'   `reglas_aplicadas` y `columnas_objetivo`.
#' @export
AplicarReglasFila <- function(data, col, reglas = list()) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` debe ser un data.frame.")
  }

  col_name <- rlang::as_name(rlang::ensym(col))
  if (!col_name %in% names(data)) {
    rlang::abort("`col` debe existir en `data`.")
  }

  if (!is.list(reglas)) {
    rlang::abort("`reglas` debe ser una lista.")
  }

  reglas_normalizadas <- lapply(reglas, function(regla) {
    if (inherits(regla, "regla_fila")) {
      return(regla)
    }

    if (!is.list(regla) || length(regla) == 0) {
      rlang::abort("Cada elemento de `reglas` debe ser `regla_fila` o una lista valida.")
    }

    etiqueta <- as.character(regla[[1]])[1]
    ReglaFila(
      etiqueta = etiqueta,
      fmt = regla$fmt,
      negrita = .valor_por_defecto(regla$negrita, TRUE),
      cursiva = .valor_por_defecto(regla$cursiva, FALSE),
      color = .valor_por_defecto(regla$color, "#000000"),
      alineacion = .valor_por_defecto(regla$alineacion, "left"),
      tamano_pct = .valor_por_defecto(regla$tamano_pct, 1),
      meta = .valor_por_defecto(regla$meta, NA),
      prop = .valor_por_defecto(regla$prop, TRUE),
      clase = .valor_por_defecto(regla$clase, character()),
      columnas = regla$columnas
    )
  })

  n <- nrow(data)
  estilos <- vector("list", n)
  clases <- vector("list", n)
  aplicadas <- vector("list", n)
  cols_target <- vector("list", n)
  out_data <- data

  for (i in seq_len(n)) {
    estilos_i <- list()
    clases_i <- character()
    aplicadas_i <- character()
    cols_i <- character()

    etiqueta_fila <- as.character(data[[col_name]][i])
    reglas_i <- Filter(function(r) identical(etiqueta_fila, r$etiqueta), reglas_normalizadas)

    for (regla in reglas_i) {
      est <- .estilo_desde_regla(regla)
      estilos_i <- utils::modifyList(estilos_i, est$estilo)
      clases_i <- unique(c(clases_i, regla$clase))
      aplicadas_i <- c(aplicadas_i, regla$etiqueta)

      columnas_obj <- regla$columnas
      if (is.null(columnas_obj)) {
        columnas_obj <- names(data)[vapply(data, is.numeric, logical(1))]
        columnas_obj <- setdiff(columnas_obj, col_name)
      }
      columnas_obj <- intersect(columnas_obj, names(data))
      cols_i <- unique(c(cols_i, columnas_obj))

      if (!is.null(regla$fmt)) {
        for (nm in columnas_obj) {
          valor <- data[[nm]][i]
          if (!is.numeric(valor)) {
            next
          }
          html_num <- as.character(FormatearNumero(
            x = valor,
            formato = regla$fmt,
            negrita = regla$negrita,
            color = regla$color,
            meta = regla$meta,
            prop = regla$prop
          ))
          out_data[[nm]][i] <- as.character(.aplicar_estilo_html(html_num, est$style_txt))
        }
      }
    }

    estilos[[i]] <- estilos_i
    clases[[i]] <- clases_i
    aplicadas[[i]] <- aplicadas_i
    cols_target[[i]] <- cols_i
  }

  list(
    data = out_data,
    estilos_fila = estilos,
    clases_fila = vapply(clases, paste, collapse = " ", FUN.VALUE = character(1)),
    reglas_aplicadas = aplicadas,
    columnas_objetivo = cols_target
  )
}

#' Fachada de una sola llamada para reglas por fila
#'
#' @inheritParams AplicarReglasFila
#' @return Misma estructura que `AplicarReglasFila()`.
#' @export
TablaFilas <- function(data, col, reglas = list()) {
  AplicarReglasFila(data = data, col = {{ col }}, reglas = reglas)
}
