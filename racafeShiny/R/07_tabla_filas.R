# Utilidad independiente para colDef con HTML — no depende del dato
DefinirColumnaHtml <- function(etiqueta, alineacion = "right", def_extra = list()) {
  do.call(colDef, modifyList(list(name = etiqueta, align = alineacion, html = TRUE), def_extra))
}

FormatearFila <- function(df, reglas = list(), col_item = "Item") {
  regla_defecto <- list(formato = "coma", negrita = TRUE, color = "#000000", meta = NA, prop = TRUE)
  obtener_regla <- function(item) modifyList(regla_defecto, reglas[[item]] %||% list())

  # Formateo numérico con HTML por fila según reglas declaradas
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
