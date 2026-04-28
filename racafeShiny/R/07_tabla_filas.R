# ----------------------------------------------------------------------------
# Utilidades para construir tablas con formato por fila.
# ----------------------------------------------------------------------------

#' Definir una columna HTML para `reactable`
#'
#' Crea una definición de columna (`colDef`) con soporte HTML.
#' Esta utilidad es independiente de los datos: solo configura la columna.
#'
#' @param etiqueta Etiqueta visible de la columna.
#' @param alineacion Alineación del contenido de la columna.
#' @param def_extra Lista de opciones adicionales para `reactable::colDef`.
#' @return Objeto de definición de columna para `reactable`.
#' @export
# Crea una definición de columna (`colDef`) con soporte HTML.
# Esta utilidad es independiente de los datos: solo configura la columna.
DefinirColumnaHtml <- function(etiqueta, alineacion = "right", def_extra = list()) {
  definicion_base <- list(name = etiqueta, align = alineacion, html = TRUE)
  do.call(colDef, modifyList(definicion_base, def_extra))
}

#' Obtener regla de formato por fila
#'
#' Devuelve la regla de formato aplicable a un elemento (`item`) de la tabla.
#'
#' @param item Identificador del ítem/fila.
#' @param reglas Lista nombrada de reglas por ítem.
#' @return Lista con la regla final (completando valores por defecto).
#' @export
# Devuelve la regla de formato aplicable a un `item`.
ObtenerReglaFila <- function(item, reglas) {
  regla_defecto <- list(
    formato = "coma",
    negrita = TRUE,
    color = "#000000",
    meta = NA,
    prop = TRUE
  )

  modifyList(regla_defecto, reglas[[item]] %||% list())
}

#' Formatear filas numéricas según reglas por ítem
#'
#' @param df `data.frame` origen.
#' @param reglas Lista nombrada por valor de `col_item`.
#' @param col_item Nombre de columna que identifica la fila para buscar su regla.
#' @return `data.frame` con columnas numéricas formateadas como HTML.
#' @export
# Formatea los valores numéricos de una tabla según la regla definida por fila.
#
# - `df`: data frame origen.
# - `reglas`: lista nombrada por valor de `col_item`.
# - `col_item`: nombre de columna que identifica la fila para buscar su regla.
FormatearFila <- function(df, reglas = list(), col_item = "Item") {
  df %>%
    mutate(
      across(
        where(is.numeric),
        ~ map2(.x = ., .y = .data[[col_item]], function(valor, item) {
          if (is.na(valor)) {
            return(NA)
          }

          regla <- ObtenerReglaFila(item = item, reglas = reglas)

          racafeShiny::FormatearNumero(
            x = valor,
            formato = regla$formato,
            negrita = regla$negrita,
            color = regla$color,
            meta = regla$meta,
            prop = regla$prop
          )
        })
      )
    )
}
