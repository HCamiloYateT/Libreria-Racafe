# ============================================================
# Seccion 06 — Formatos y estilos
# Registro de formatos, formateo numerico/texto y estilos gt
# ============================================================


# ---- Registro de formatos ----

#' Definir formato preconfigurado
#' @param formato Nombre del formato.
#' @param fn Funcion de formato personalizada (opcional).
#' @param ... Argumentos adicionales pasados al generador de `scales`.
#' @return Funcion de formato.
#' @export
DefinirFormato <- function(formato, fn = NULL, ...) {
  if (missing(formato) || length(formato) == 0L || is.na(formato[1])) {
    stop("Debe especificar un formato valido.", call. = FALSE)
  }

  formato <- tolower(trimws(as.character(formato[1])))

  if (!is.null(fn)) {
    if (!is.function(fn)) {
      stop("`fn` debe ser una funcion de formato.", call. = FALSE)
    }
    assign(formato, fn, envir = .formatos_racafe)
    return(invisible(fn))
  }

  generadores <- list(
    coma = function(...) scales::label_number(accuracy = 1, big.mark = ",", ...),
    numero = function(...) scales::label_number(accuracy = 0.01, big.mark = ",", ...),
    dinero = function(...) scales::label_number(accuracy = 1, prefix = "$", big.mark = ",", ...),
    dolares = function(...) scales::label_number(accuracy = 0.01, prefix = "$", big.mark = ",", ...),
    miles = function(...) scales::label_number(
      accuracy = 0.01, scale = 0.001, prefix = "$", big.mark = ",", ...
    ),
    miles0 = function(...) scales::label_number(
      accuracy = 1, scale = 0.001, prefix = "$", big.mark = ",", ...
    ),
    porcentaje = function(...) scales::label_number(
      accuracy = 0.01, scale = 100, suffix = "%", big.mark = ",", ...
    ),
    cientifico = function(...) scales::label_scientific(...),
    millones = function(...) scales::label_number(
      accuracy = 0.01, scale = 0.000001, prefix = "$", suffix = " M", big.mark = ",", ...
    ),
    entero = function(...) scales::label_number(accuracy = 1, big.mark = ",", ...),
    tiempo = function(...) scales::label_time(...),
    kwh = function(...) scales::label_number(
      accuracy = 0.01, suffix = " kWh", big.mark = ",", ...
    ),
    log = function(...) scales::label_log(...)
  )

  if (!formato %in% names(generadores)) {
    stop(
      paste0(
        "Formato no reconocido. Use: 'coma', 'numero', 'dinero', 'dolares', ",
        "'miles', 'miles0', 'porcentaje', 'cientifico', 'millones', 'entero', ",
        "'tiempo', 'kwh' o 'log'."
      ),
      call. = FALSE
    )
  }

  fn <- generadores[[formato]](...)
  assign(formato, fn, envir = .formatos_racafe)
  invisible(fn)
}

#' Obtener un formato registrado por nombre
#' @param nombre Nombre del formato.
#' @return Funcion de formato.
#' @export
ObtenerFormato <- function(nombre) {
  fn <- get0(nombre, envir = .formatos_racafe, inherits = FALSE)
  if (is.null(fn)) {
    stop(sprintf(
      "Formato '%s' no registrado. Disponibles: %s",
      nombre, paste(ls(envir = .formatos_racafe), collapse = ", ")
    ), call. = FALSE)
  }
  fn
}

#' Listar formatos registrados
#' @return Vector de caracteres con nombres disponibles.
#' @export
ListarFormatos <- function() ls(envir = .formatos_racafe)


#' Convertir un formato registrado a sintaxis D3.js
#'
#' @param formato Nombre del formato registrado.
#' @return Cadena con la especificacion D3 (ej. `",.0f"`).
#' @export
FormatoD3 <- function(formato) {
  mapa <- list(
    coma       = ",.0f",
    numero     = ",.2f",
    dinero     = "$,.0f",
    dolares    = "$,.2f",
    miles      = "$,.2s",
    miles0     = "$,.0f",
    porcentaje = ".2%",
    cientifico = ".2e",
    millones   = "$,.2s",
    entero     = ",.0f",
    tiempo     = ".2f",
    kwh        = ",.2f",
    log        = ".2f",
    decimal    = ",.2f",
    variacion  = "+.1%"
  )
  mapa[[formato]] %||% ",.2f"
}


#' Convertir un formato registrado a representacion JavaScript
#'
#' @param formato Nombre del formato registrado.
#' @return Cadena con funcion JS como string.
#' @export
FormatoJS <- function(formato) {
  d3 <- FormatoD3(formato)
  sprintf("d3.format('%s')", d3)
}


#' Convertir un formato registrado a sintaxis Handsontable
#'
#' @param formato Nombre del formato registrado.
#' @return Cadena con el patron de formato para Handsontable.
#' @export
FormatoHOT <- function(formato) {
  mapa <- list(
    coma       = "0,0",
    numero     = "0,0.00",
    dinero     = "$0,0",
    dolares    = "$0,0.00",
    miles      = "$0,0.00",
    miles0     = "$0,0",
    porcentaje = "0.00%",
    cientifico = "0.00E+00",
    millones   = "$0,0.00",
    entero     = "0,0",
    tiempo     = "hh:mm:ss",
    kwh        = "0,0.00",
    log        = "0,0.00",
    decimal    = "0,0.00",
    variacion  = "+0.0%"
  )
  mapa[[formato]] %||% "0,0.00"
}


# ---- Formateo de valores ----

#' Aplicar formato numerico con estilos HTML condicionales
#'
#' Genera HTML con color y negrita segun cumplimiento de meta.
#'
#' @param x Valor numerico.
#' @param formato Nombre del formato registrado.
#' @param negrita Logico. Aplicar negrita.
#' @param color Color por defecto del texto.
#' @param meta Valor de meta para colorear condicionalmente. `NA` desactiva.
#' @param prop Logico. Si `TRUE`, `x` y `meta` son proporciones (cumplimiento).
#' @return Cadena HTML con el valor formateado.
#' @export
#' @examples
#' FormatearNumero(0.85, "porcentaje", meta = 0.80)
#' FormatearNumero(1250000, "dinero")
FormatearNumero <- function(
    x,
    formato,
    negrita = TRUE,
    color   = "#000000",
    meta    = NA,
    prop    = TRUE) {

  fn    <- ObtenerFormato(formato)
  texto <- fn(x)

  if (!is.na(meta)) {
    cumple <- if (prop) x >= meta else x >= meta
    color <- if (cumple) "#1A7A5E" else "#C0392B"
  }

  estilo <- sprintf(
    "color:%s;%s",
    color,
    if (negrita) "font-weight:600;" else ""
  )

  htmltools::HTML(sprintf('<span style="%s">%s</span>', estilo, texto))
}


#' Aplicar estilo HTML a texto plano
#'
#' @param x Texto a estilizar.
#' @param negrita Logico. Aplicar negrita.
#' @param color Color del texto.
#' @param tamano_pct Multiplicador del tamano de fuente relativo (1 = 100%).
#' @param alineacion Alineacion del texto: `"left"`, `"center"`, `"right"`.
#' @param transform Transformacion CSS: `"none"`, `"uppercase"`, `"lowercase"`.
#' @return Cadena HTML con el texto estilizado.
#' @export
#' @examples
#' FormatearTexto("Meta alcanzada", color = "#28B78D")
#' FormatearTexto("URGENTE", negrita = TRUE, transform = "uppercase")
FormatearTexto <- function(
    x,
    negrita    = TRUE,
    color      = "#000000",
    tamano_pct = 1,
    alineacion = "left",
    transform  = "none") {

  estilo <- sprintf(
    "color:%s;font-size:%s%%;text-align:%s;text-transform:%s;%s",
    color,
    round(tamano_pct * 100),
    alineacion,
    transform,
    if (negrita) "font-weight:600;" else ""
  )

  htmltools::HTML(sprintf('<span style="%s">%s</span>', estilo, x))
}

#' Alias de compatibilidad para `FormatearTexto`
#' @inheritParams FormatearTexto
#' @return Cadena HTML con el texto estilizado.
#' @export
FormatrearTexto <- FormatearTexto


# ---- Estilos para tablas gt ----

#' Aplicar estilo minimalista a tabla gt
#'
#' @param gt_table Objeto `gt_tbl`.
#' @return Objeto `gt_tbl` con estilo aplicado.
#' @export
gt_minimal_style <- function(gt_table) {
  gt_table |>
    gt::tab_options(
      table.font.size          = gt::px(13),
      table.border.top.style   = "none",
      table.border.bottom.style = "none",
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = gt::px(1),
      column_labels.border.top.color = "#CCCCCC",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "#CCCCCC",
      row.striping.background_color = "#F9F9F9",
      row.striping.include_table_body = TRUE,
      data_row.padding = gt::px(5)
    ) |>
    gt::opt_row_striping() |>
    gt::opt_table_font(font = gt::google_font("Roboto"))
}


#' Generar tabla gt de mensaje vacio
#'
#' Util para mostrar en Shiny cuando no hay datos disponibles.
#'
#' @param mensaje Texto a mostrar.
#' @return Objeto `gt_tbl` minimalista con el mensaje.
#' @export
#' @examples
#' gt_mensaje_vacio()
#' gt_mensaje_vacio("Sin resultados para los filtros seleccionados")
gt_mensaje_vacio <- function(mensaje = "No existen datos en la tabla") {
  df_vacio <- data.frame(Mensaje = mensaje)
  df_vacio |>
    gt::gt() |>
    gt::cols_label(Mensaje = "") |>
    gt::tab_options(
      column_labels.hidden      = TRUE,
      table.border.top.style    = "none",
      table.border.bottom.style = "none"
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        color  = "#888888",
        style  = "italic",
        size   = gt::px(13),
        align  = "center"
      ),
      locations = gt::cells_body(columns = dplyr::everything())
    )
}


#' Colores KPI segun cumplimiento
#'
#' @param x Vector numerico de cumplimiento (tipicamente proporciones, meta = 1).
#' @param prop Logico. Si `TRUE`, evalua como proporcion respecto a 1.
#' @return Vector de colores hexadecimales.
#' @export
#' @examples
#' col_kpi(c(0.7, 1.0, 1.2))
col_kpi <- function(x, prop = TRUE) {
  dplyr::case_when(
    is.na(x)              ~ "#CCCCCC",
    prop & x >= 1         ~ "#1A7A5E",
    prop & x >= 0.9       ~ "#F39C12",
    prop & x < 0.9        ~ "#C0392B",
    !prop & x > 0         ~ "#1A7A5E",
    !prop & x == 0        ~ "#888888",
    TRUE                  ~ "#C0392B"
  )
}


#' Indicadores textuales de desempeno KPI
#'
#' @param x Vector numerico de cumplimiento.
#' @return Vector de caracteres con flechas indicadoras HTML.
#' @export
#' @examples
#' chr_kpi(c(0.7, 1.0, 1.2))
chr_kpi <- function(x) {
  dplyr::case_when(
    is.na(x)       ~ "\u2014",
    x >= 1         ~ "\u2714",
    x >= 0.9       ~ "\u26A0",
    TRUE           ~ "\u2716"
  )
}


#' Paleta numerica estandar del paquete
#'
#' @param x Vector numerico para mapear a colores.
#' @return Vector de colores hexadecimales (escala verde-azul).
#' @export
#' @examples
#' col_num(1:5)
col_num <- function(x) {
  ColoresGreenBlue(x)
}


#' Formato de porcentaje en columnas gt
#'
#' @param gt_table Objeto `gt_tbl`.
#' @param ... Columnas seleccionadas (tidy-select).
#' @return Objeto `gt_tbl` con formato aplicado.
#' @export
gt_pct_style <- function(gt_table, ...) {
  gt_table |>
    gt::fmt_percent(
      columns  = c(...),
      decimals = 1,
      dec_mark = ",",
      sep_mark = "."
    )
}


#' Formato variacional (+/-) en columnas gt
#'
#' @param gt_table Objeto `gt_tbl`.
#' @param ... Columnas seleccionadas.
#' @return Objeto `gt_tbl` con formato variacional.
#' @export
gt_var_style <- function(gt_table, ...) {
  gt_table |>
    gt::fmt_percent(
      columns     = c(...),
      decimals    = 1,
      force_sign  = TRUE,
      dec_mark    = ",",
      sep_mark    = "."
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "#1A7A5E"),
      locations = gt::cells_body(
        columns = c(...),
        rows    = {{ ... }} > 0
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "#C0392B"),
      locations = gt::cells_body(
        columns = c(...),
        rows    = {{ ... }} < 0
      )
    )
}


#' Colorear columnas gt segun signo del valor
#'
#' Positivos en verde, negativos en rojo, ceros en negro.
#'
#' @param gt_table Objeto `gt_tbl`.
#' @param ... Columnas seleccionadas.
#' @return Objeto `gt_tbl` con estilo de signo.
#' @export
gt_sign_style <- function(gt_table, ...) {
  cols_expr <- rlang::enexprs(...)

  for (col in cols_expr) {
    gt_table <- gt_table |>
      gt::tab_style(
        style = gt::cell_text(color = "#1A7A5E"),
        locations = gt::cells_body(
          columns = !!col,
          rows    = !!col > 0
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(color = "#C0392B"),
        locations = gt::cells_body(
          columns = !!col,
          rows    = !!col < 0
        )
      )
  }
  gt_table
}


#' Colorear columnas especificas en gt con un color fijo
#'
#' @param gt_table Objeto `gt_tbl`.
#' @param columns Columnas a colorear (tidy-select).
#' @param color Color hexadecimal a aplicar.
#' @return Objeto `gt_tbl` con columnas coloreadas.
#' @export
#' @examples
#' \dontrun{
#'   gt_color_columns(gt::gt(head(mtcars)), columns = hp, color = "#28B78D")
#' }
gt_color_columns <- function(gt_table, columns, color) {
  gt_table |>
    gt::tab_style(
      style     = gt::cell_text(color = color),
      locations = gt::cells_body(columns = {{ columns }})
    )
}
