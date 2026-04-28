# FORMATEO DE FILAS ----
# Soporte de clave ".default" en reglas para definir la base de items no declarados explícitamente
FormatearFila <- function(df, reglas = list(), col_item = "Item") {
  regla_sistema <- list(formato = "coma", negrita = TRUE, color = "#000000", meta = NA, prop = TRUE)

  # La clave ".default" en reglas sobreescribe la regla del sistema para items no declarados
  regla_base    <- modifyList(regla_sistema, reglas[[".default"]] %||% list())
  obtener_regla <- function(item) modifyList(regla_base, reglas[[item]] %||% list())

  df %>%
    mutate(across(where(is.numeric), ~ {
      map2(.x = ., .y = .data[[col_item]], function(val, it) {
        if (is.na(val)) return(NA)
        r <- obtener_regla(it)
        racafeShiny::FormatearNumero(x = val, formato = r$formato, negrita = r$negrita,
                                     color = r$color, meta = r$meta, prop = r$prop)
      })
    }))
}

# GENERACIÓN DE COL_SPECS ----
# Produce automáticamente colDef HTML para columnas numéricas del dataframe,
# respetando overrides por nombre y protegiendo columnas de texto ya declaradas
GenerarColSpecsHtml <- function(df,
                                overrides   = list(),
                                excluir     = character(0),
                                alineacion  = "right") {
  cols_auto <- df %>%
    select(where(is.numeric)) %>%
    names() %>%
    setdiff(excluir)

  lapply(setNames(cols_auto, cols_auto), function(col) {
    args_extra <- overrides[[col]] %||% list()
    do.call(DefinirColumnaHtml, modifyList(
      list(etiqueta = col, alineacion = alineacion),
      args_extra
    ))
  })
}
