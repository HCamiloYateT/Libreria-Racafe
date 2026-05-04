# ============================================================
# Seccion 08 — Componentes Shiny: Inputs
# Controles de entrada con estilo corporativo Racafe
# ============================================================


# ---- Inputs numericos ----

#' Input numerico personalizado con layout de columnas
#'
#' @param id ID del input Shiny.
#' @param label Etiqueta visible del input.
#' @param value Valor inicial.
#' @param dec Numero de decimales permitidos.
#' @param max Valor maximo permitido. `NULL` sin limite.
#' @param min Valor minimo permitido. `NULL` sin limite.
#' @param type Nombre del formato registrado para mostrar (solo visual).
#' @param label_col Ancho de la columna del label (sobre 12).
#' @param input_col Ancho de la columna del input (sobre 12).
#' @param width Ancho CSS del input.
#' @param ns Funcion namespace de modulo Shiny. `NULL` si no es modulo.
#' @return `tagList` de Shiny con el input numerico.
#' @export
#' @examples
#' \dontrun{
#'   InputNumerico("ventas", "Ventas", 1000, dec = 0)
#' }
InputNumerico <- function(
    id,
    label,
    value,
    dec       = 2,
    max       = NULL,
    min       = NULL,
    type      = "numero",
    label_col = 6,
    input_col = 6,
    width     = "100%",
    ns        = NULL) {

  input_id <- if (!is.null(ns)) ns(id) else id

  shiny::fluidRow(
    shiny::column(
      width = label_col,
      shiny::tags$label(
        `for` = input_id,
        class = "control-label racafe-label",
        label
      )
    ),
    shiny::column(
      width = input_col,
      shiny::numericInput(
        inputId = input_id,
        label   = NULL,
        value   = value,
        min     = min %||% NA,
        max     = max %||% NA,
        step    = 10^(-dec),
        width   = width
      )
    )
  )
}


# ---- Selectores ----

#' Lista desplegable con pickerInput estilizado
#'
#' @param inputId ID del input.
#' @param label Etiqueta. `NULL` sin etiqueta.
#' @param choices Opciones disponibles.
#' @param selected Opciones seleccionadas por defecto.
#' @param multiple Logico. Permitir seleccion multiple.
#' @param fem Logico. Usar articulos en femenino en el placeholder.
#' @param ns Funcion namespace de modulo Shiny.
#' @return Objeto `pickerInput` de shinyWidgets.
#' @export
#' @examples
#' \dontrun{
#'   ListaDesplegable("region", "Region", choices = c("Norte", "Sur"))
#' }
ListaDesplegable <- function(
    inputId,
    label    = NULL,
    choices,
    selected = choices,
    multiple = TRUE,
    fem      = FALSE,
    ns       = NULL) {

  .check_pkg("shinyWidgets", "Shiny inputs")

  input_id  <- if (!is.null(ns)) ns(inputId) else inputId
  articulo  <- if (fem) c("Todas", "Ninguna") else c("Todos", "Ninguno")

  shinyWidgets::pickerInput(
    inputId  = input_id,
    label    = label,
    choices  = choices,
    selected = selected,
    multiple = multiple,
    options  = shinyWidgets::pickerOptions(
      actionsBox        = TRUE,
      selectAllText     = articulo[1],
      deselectAllText   = articulo[2],
      selectedTextFormat = "count > 3",
      countSelectedText = "{0} seleccionados",
      liveSearch        = length(choices) > 10,
      size              = min(10, length(choices))
    )
  )
}


#' Construir opciones con genero para pickerInput
#'
#' @param cho Vector de opciones.
#' @param fem Logico. `TRUE` para opciones en femenino.
#' @return Lista de opciones para `shinyWidgets::pickerOptions`.
#' @export
pick_opt <- function(cho, fem = TRUE) {
  articulo <- if (fem) c("Todas", "Ninguna") else c("Todos", "Ninguno")
  shinyWidgets::pickerOptions(
    actionsBox        = TRUE,
    selectAllText     = articulo[1],
    deselectAllText   = articulo[2],
    selectedTextFormat = "count > 2",
    countSelectedText = "{0} seleccionados",
    liveSearch        = length(cho) > 8
  )
}




# ---- Utils internos de botones ----

#' Valida que un valor sea un color reconocido por R
#' @keywords internal
.btn_validar_color <- function(color, nombre) {
  if (!is.character(color) || length(color) != 1 || !nzchar(color)) {
    stop(sprintf("'%s' debe ser una cadena de color no vacía.", nombre), call. = FALSE)
  }
  tryCatch(
    grDevices::col2rgb(color),
    error = function(e) {
      stop(sprintf("'%s' no es un color válido: '%s'.", nombre, color), call. = FALSE)
    }
  )
  invisible(color)
}

#' Oscurece un color R en un factor dado (0–1). Devuelve string CSS rgb().
#' @keywords internal
.btn_oscurecer <- function(color, factor = 0.18) {
  v <- grDevices::col2rgb(color)
  v <- pmax(0L, as.integer(v * (1 - factor)))
  sprintf("rgb(%d,%d,%d)", v[1L], v[2L], v[3L])
}

#' Convierte un color R a su representación CSS rgb() normalizada
#' @keywords internal
.btn_a_css <- function(color) {
  v <- grDevices::col2rgb(color)
  sprintf("rgb(%d,%d,%d)", v[1L], v[2L], v[3L])
}

#' Construye el bloque de CSS custom properties para color_fondo/fuente/hover.
#' @keywords internal
.btn_css_vars <- function(color_fondo, color_fuente, color_hover) {
  .btn_validar_color(color_fondo, "color_fondo")
  .btn_validar_color(color_fuente, "color_fuente")
  fondo_css <- .btn_a_css(color_fondo)
  fuente_css <- .btn_a_css(color_fuente)
  hover_css <- if (is.null(color_hover)) {
    .btn_oscurecer(color_fondo)
  } else {
    .btn_validar_color(color_hover, "color_hover")
    .btn_a_css(color_hover)
  }
  sprintf(
    "--racafe-color-fondo:%s;--racafe-color-fuente:%s;--racafe-color-hover:%s;background-color:%s;color:%s;border-color:%s;",
    fondo_css, fuente_css, hover_css, fondo_css, fuente_css, fondo_css
  )
}

#' Elige blanco o negro según la luminancia relativa del fondo (criterio WCAG).
#' Devuelve string CSS rgb() del color de fuente con mayor contraste.
#' @keywords internal
.btn_fuente_auto <- function(color) {
  v <- grDevices::col2rgb(color) / 255
  # Linearización sRGB -> luminancia relativa (WCAG 2.1)
  v_lin <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  L <- 0.2126 * v_lin[1L] + 0.7152 * v_lin[2L] + 0.0722 * v_lin[3L]
  if (L > 0.179) "rgb(0,0,0)" else "rgb(255,255,255)"
}

#' Grupo de botones radiales con estilo racafe
#'
#' @param inputId ID del input Shiny.
#' @param label Etiqueta superior. `NULL` para omitir.
#' @param choices Vector o lista con las opciones.
#' @param selected Opcion preseleccionada. `NULL` toma el primer elemento.
#' @param color_activo Color de fondo del botón seleccionado.
#' @param color_inactivo Color de fondo de los botones no seleccionados.
#' @param alineacion Alineacion del grupo: `"left"`, `"center"`, `"right"`.
#' @param ... Argumentos adicionales pasados a `shinyWidgets::radioGroupButtons()`.
#' @return Componente Shiny.
#' @export
BotonesRadiales <- function(
  inputId,
  label = NULL,
  choices,
  selected = NULL,
  color_activo = "#198754",
  color_inactivo = "#6c757d",
  alineacion = c("left", "center", "right"),
  ...
) {
  .check_pkg("shinyWidgets", "BotonesRadiales")
  alineacion <- match.arg(alineacion)

  .btn_validar_color(color_activo, "color_activo")
  .btn_validar_color(color_inactivo, "color_inactivo")

  css_vars <- sprintf(
    "--racafe-radio-inactivo:%s;--racafe-radio-fuente-inactivo:%s;--racafe-radio-hover-inactivo:%s;--racafe-radio-activo:%s;--racafe-radio-fuente-activo:%s;--racafe-radio-hover-activo:%s;",
    .btn_a_css(color_inactivo),
    .btn_fuente_auto(color_inactivo),
    .btn_oscurecer(color_inactivo),
    .btn_a_css(color_activo),
    .btn_fuente_auto(color_activo),
    .btn_oscurecer(color_activo)
  )

  clase_alineacion <- switch(
    alineacion,
    left   = "justify-content-start",
    center = "justify-content-center",
    right  = "justify-content-end"
  )

  shiny::div(
    class = paste("racafe-radio-group", clase_alineacion),
    style = css_vars,
    shinyWidgets::radioGroupButtons(
      inputId  = inputId,
      label    = label,
      choices  = choices,
      selected = selected %||% choices[[1]],
      ...
    )
  )
}


#' Boton interruptor (toggle switch)
#'
#' @param ... Argumentos pasados a `shinyWidgets::materialSwitch`.
#' @return Componente Shiny.
#' @export
#' @examples
#' \dontrun{
#'   BotonEstado("toggle_filtro", "Activar filtro")
#' }
BotonEstado <- function(...) {
  .check_pkg("shinyWidgets", "Shiny inputs")
  shinyWidgets::materialSwitch(
    ...,
    status = "success",
    right  = TRUE
  )
}


#' Selector de fecha con configuracion derivada por tipo de vista
#'
#' Wrapper de [shinyWidgets::airDatepickerInput()] que deriva automaticamente
#' `view`, `minView`, `dateFormat`, `startView` y constantes de idioma a
#' partir del tipo de granularidad solicitada. Solo requiere los parametros
#' sustantivos: id, valor inicial, tipo y rango.
#'
#' @param id String. `inputId` del widget.
#' @param label String o `NULL`. Etiqueta visible. `NULL` la omite.
#' @param value Date. Fecha inicial seleccionada. Default `Sys.Date()`.
#' @param tipo String. Granularidad: `"dia"`, `"mes"`, `"anio"`.
#' @param min_date Date o `NULL`. Fecha minima seleccionable. `NULL` sin limite.
#' @param max_date Date o `NULL`. Fecha maxima seleccionable. `NULL` sin limite.
#' @param inline Logical. `TRUE` muestra el calendario embebido (sin dropdown).
#'   Default `FALSE`.
#' @param width String. Ancho CSS del input. Default `"100%"`.
#' @param ... Argumentos adicionales pasados a `airDatepickerInput()`.
#'
#' @return `shiny.tag` del widget `airDatepickerInput` configurado.
#' @export
#'
#' @examples
#' InputFecha("fecha_dia",  label = "Dia",  tipo = "dia")
#' InputFecha("fecha_mes",  label = "Mes",  tipo = "mes",  value = Sys.Date(),
#'            min_date = as.Date("2023-01-01"), max_date = Sys.Date())
#' InputFecha("fecha_anio", label = "Anio", tipo = "anio", inline = TRUE)
InputFecha <- function(
    id,
    label    = NULL,
    value    = Sys.Date(),
    tipo     = c("dia", "mes", "anio"),
    min_date = NULL,
    max_date = NULL,
    inline   = FALSE,
    width    = "100%",
    ...) {

  .check_pkg("shinyWidgets", "Shiny inputs")
  tipo <- match.arg(tipo)

  if (!inherits(value, "Date")) {
    value <- as.Date(value)
  }
  if (is.na(value)) {
    stop("`value` no es una fecha valida.", call. = FALSE)
  }

  if (!is.null(min_date) && !inherits(min_date, "Date")) min_date <- as.Date(min_date)
  if (!is.null(max_date) && !inherits(max_date, "Date")) max_date <- as.Date(max_date)

  if (!is.null(min_date) && is.na(min_date)) {
    stop("`min_date` no es una fecha valida.", call. = FALSE)
  }
  if (!is.null(max_date) && is.na(max_date)) {
    stop("`max_date` no es una fecha valida.", call. = FALSE)
  }

  if (!is.null(min_date) && value < min_date) value <- min_date
  if (!is.null(max_date) && value > max_date) value <- max_date

  params <- switch(
    tipo,
    dia = list(
      view       = "days",
      minView    = "days",
      dateFormat = "yyyy-MM-dd",
      startView  = value
    ),
    mes = list(
      view       = "months",
      minView    = "months",
      dateFormat = "yyyy-MM",
      startView  = lubridate::floor_date(value, "month")
    ),
    anio = list(
      view       = "years",
      minView    = "years",
      dateFormat = "yyyy",
      startView  = lubridate::floor_date(value, "year")
    )
  )

  shinyWidgets::airDatepickerInput(
    inputId     = id,
    label       = label,
    value       = value,
    view        = params$view,
    minView     = params$minView,
    dateFormat  = params$dateFormat,
    startView   = params$startView,
    minDate     = min_date,
    maxDate     = max_date,
    monthsField = "monthsShort",
    language    = "es",
    inline      = inline,
    width       = width,
    ...
  )
}



#' Selector de mes (wrapper de `InputFecha`)
#'
#' @inheritParams InputFecha
#' @return `shiny.tag` del widget `airDatepickerInput` configurado para meses.
#' @export
InputMes <- function(id, label = NULL, value = Sys.Date(),
                     min_date = NULL, max_date = NULL,
                     inline = FALSE, width = "100%", ...) {
  InputFecha(id = id, label = label, value = value,
             tipo = "mes", min_date = min_date, max_date = max_date,
             inline = inline, width = width, ...)
}

#' Boton de accion generico con alineacion configurable
#'
#' @param id ID del boton.
#' @param label Texto del boton. Use `NULL` para omitir texto.
#' @param icono Nombre del icono Font Awesome. Use `NULL` para omitir icono.
#' @param align Alineacion: `"left"`, `"center"`, `"right"`.
#' @param size Tamano visual del boton: `"xxs"`, `"xs"`, `"sm"`, `"md"`, `"lg"`, `"xl"`, `"xxl"`.
#' @param color_fondo Color de fondo base (nombre R o hex CSS).
#' @param color_fuente Color de texto/icono.
#' @param color_hover Color de fondo al hover. `NULL` aplica oscurecimiento automatico.
#' @param label_posicion Posicion del label respecto al icono: `"right"` o `"below"`.
#' @param titulo Tooltip del boton al pasar el cursor. `NULL` para omitir.
#' @param ... Argumentos adicionales para `shiny::actionButton`.
#' @return Componente Shiny.
#' @export
#' @examples
#' \dontrun{
#'   Boton("guardar_form", label = "Guardar cambios", align = "center")
#'   Boton("guardar_icono", label = NULL, icono = "floppy-disk")
#'   Boton("guardar_completo", label = "Guardar", icono = "floppy-disk")
#'   Boton("guardar_abajo", label = "Guardar", icono = "floppy-disk", label_posicion = "below")
#' }
Boton <- function(
    id,
    label          = "Guardar",
    icono          = "floppy-disk",
    align          = "right",
    size           = "sm",
    color_fondo    = "#198754",
    color_fuente   = "#FFFFFF",
    color_hover    = NULL,
    label_posicion = "right",
    titulo         = NULL,
    ...) {

  align          <- match.arg(align, c("left", "center", "right"))
  size           <- match.arg(size, c("xxs", "xs", "sm", "md", "lg", "xl", "xxl"))
  label_posicion <- match.arg(label_posicion, c("right", "below"))

  if (is.null(label) && is.null(icono)) {
    stop("Debe especificar al menos `label` o `icono`.", call. = FALSE)
  }
  if (!is.null(titulo) && (!is.character(titulo) || length(titulo) != 1)) {
    stop("`titulo` debe ser `NULL` o una cadena de longitud 1.", call. = FALSE)
  }

  css_vars <- .btn_css_vars(color_fondo, color_fuente, color_hover)

  clase_align <- switch(
    align,
    left   = "text-left",
    center = "text-center",
    right  = "text-right"
  )

  clase_label <- if (!is.null(icono) && !is.null(label) && identical(label_posicion, "below")) {
    "racafe-btn-content racafe-btn-content--column"
  } else {
    "racafe-btn-content"
  }

  contenido_boton <- shiny::tags$span(
    class = clase_label,
    if (!is.null(icono)) shiny::icon(icono, class = "racafe-btn-icon") else NULL,
    if (!is.null(label)) shiny::tags$span(class = "racafe-btn-label", label) else NULL
  )

  clase_boton <- paste(c(
    "btn racafe-btn racafe-btn-guardar",
    sprintf("racafe-btn-guardar--%s", size),
    "racafe-btn-content-host",
    if (identical(label_posicion, "below")) "racafe-btn-content-host--column" else NULL
  ), collapse = " ")

  boton <- shiny::actionButton(
    inputId = id,
    label   = contenido_boton,
    class   = clase_boton,
    icon    = NULL,
    style   = css_vars,
    ...
  )

  boton <- shiny::tagAppendAttributes(
    boton,
    `data-racafe-label-pos` = label_posicion,
    title = titulo
  )

  shiny::div(
    class = clase_align,
    boton
  )
}
