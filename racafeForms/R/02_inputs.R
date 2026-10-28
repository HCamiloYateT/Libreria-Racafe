#' Entrada de texto corporativa
#' @param id Identificador local; ns aplica namespace una sola vez.
#' @param label Etiqueta.
#' @param value Valor inicial.
#' @param placeholder Texto de ayuda dentro del campo.
#' @param type text, email, tel o password.
#' @param required Indica obligatoriedad para accesibilidad (la validacion es del servidor).
#' @param help Ayuda debajo del control.
#' @param width Ancho CSS.
#' @param ns Funcion namespace de modulo.
#' @param readonly Impedir edición por teclado.
#' @param disabled Deshabilitar el control.
#' @param error Mensaje de validación del campo, o NULL.
#' @param autocomplete Token de autocompletado HTML; NULL conserva el predeterminado.
#' @param choices Catálogo opcional de sugerencias de texto; no limita la validación del servidor.
#' @return Componente de entrada Shiny para incluir en la UI; su valor está disponible en `input$id`.
#' @export
racafeInputText <- function(id, label, value = "", placeholder = NULL,
                            type = c("text", "email", "tel", "password"),
                            required = FALSE, help = NULL, width = "100%", ns = NULL,
                            readonly = FALSE, disabled = FALSE, error = NULL, autocomplete = NULL, choices = NULL) {
  type <- match.arg(type)
  tag <- if (type == "password") {
    shiny::passwordInput(.rf_id(id, ns), label, value = value, placeholder = placeholder, width = width)
  } else {
    shiny::textInput(.rf_id(id, ns), label, value = value, placeholder = placeholder, width = width)
  }
  if (type %in% c("email", "tel")) {
    tag <- htmltools::tagQuery(tag)$find("input")$addAttrs(type = type)$allTags()
  }
  if (!is.null(autocomplete)) {
    tag <- htmltools::tagQuery(tag)$find("input")$addAttrs(autocomplete = autocomplete)$allTags()
  }
  if (!is.null(choices)) {
    catalog_id <- .rf_id(paste0(id, "_catalog"), ns)
    tag <- htmltools::tagQuery(tag)$find("input")$addAttrs(list = catalog_id)$allTags()
    tag <- shiny::tagAppendChildren(tag, shiny::tags$datalist(id = catalog_id,
      lapply(as.character(.rf_choice_values(choices)), function(x) shiny::tags$option(value = x))))
  }
  .rf_input_state(tag, .rf_id(id, ns), readonly, disabled, required, help, error)
}

#' Area de texto corporativa
#' @inheritParams racafeInputText
#' @param rows Numero de filas.
#' @param maxlength Maximo de caracteres (NULL sin limite).
#' @return Componente de entrada Shiny para incluir en la UI; su valor está disponible en `input$id`.
#' @export
racafeInputTextArea <- function(id, label, value = "", placeholder = NULL, rows = 4,
                                maxlength = NULL, required = FALSE, help = NULL,
                                width = "100%", ns = NULL, readonly = FALSE, disabled = FALSE, error = NULL) {
  tag <- shiny::textAreaInput(.rf_id(id, ns), label, value = value,
                              placeholder = placeholder, rows = rows, width = width)
  if (!is.null(maxlength)) {
    tag <- htmltools::tagQuery(tag)$find("textarea")$addAttrs(maxlength = maxlength)$allTags()
  }
  .rf_input_state(tag, .rf_id(id, ns), readonly, disabled, required, help, error)
}

#' ListaDesplegable corporativa basada en pickerInput
#' @param id ID local del campo.
#' @param label Etiqueta, o NULL.
#' @param choices Opciones Shiny, incluidas opciones con nombres y valores.
#' @param selected Valor seleccionado; por defecto vacio en seleccion simple.
#' @param multiple Permite seleccionar varias opciones.
#' @param search Mostrar busqueda.
#' @param empty Agrega opcion Ninguno con valor vacio, solo para seleccion simple.
#' @param empty_label Etiqueta sin selección; por defecto concuerda con `gender`.
#' @param gender Concordancia del resumen múltiple: `"m"` o `"f"`.
#' @details Una selección muestra su etiqueta; varias muestran el conteo y todas, Todos o Todas.
#'   El resumen no modifica los valores enviados a Shiny.
#' @param size Numero de elementos visibles en lista.
#' @param dropup_auto Determinar automaticamente si abrir hacia arriba.
#' @param width Ancho CSS.
#' @param ns Namespace del modulo.
#' @return Componente de entrada Shiny para incluir en la UI; su valor está disponible en `input$id`.
#' @param disabled Deshabilitar el control.
#' @param required Marcar el campo como obligatorio.
#' @param help Ayuda opcional del campo.
#' @param error Mensaje de validación opcional.
#' @export
racafeInputSelect <- function(id, label = NULL, choices, selected = NULL, multiple = FALSE,
                              search = TRUE, empty = TRUE, empty_label = "Ninguno", size = 6,
                              dropup_auto = FALSE, width = "100%", ns = NULL, gender = c("m", "f"),
                              disabled = FALSE, required = FALSE, help = NULL, error = NULL) {
  gender <- match.arg(gender)
  if (missing(empty_label)) empty_label <- if (gender == "f") "Ninguna" else "Ninguno"
  if (!multiple && empty && !"" %in% as.character(.rf_choice_values(choices))) {
    choices <- c(stats::setNames("", empty_label), choices)
  }
  if (is.null(selected)) selected <- if (multiple) character(0) else {
    if (empty) "" else .rf_choice_values(choices)[1]
  }
  tag <- shinyWidgets::pickerInput(
    inputId = .rf_id(id, ns), label = label, choices = choices,
    selected = selected, multiple = multiple, width = width,
    options = shinyWidgets::pickerOptions(
      liveSearch = search, dropupAuto = dropup_auto, size = size,
      noneSelectedText = empty_label, actionsBox = multiple,
      selectAllText = if (gender == "f") "Todas" else "Todos",
      deselectAllText = if (gender == "f") "Ninguna" else "Ninguno",
      selectedTextFormat = if (multiple) "count > 1" else "values"
    )
  )
  tag <- shiny::tagAppendAttributes(tag, class = "rf-select", `data-rf-gender` = gender)
  .rf_input_state(tag, .rf_id(id, ns), disabled = disabled, required = required, help = help, error = error)
}

#' Entrada numerica corporativa usando AutoNumeric
#' @param id ID local.
#' @param label Etiqueta.
#' @param value Valor inicial numerico.
#' @param format integer, number, currency, percent, factor, kg, sacks, cop, usd, o lista de opciones.
#' @param decimals Decimales; NULL usa el formato predefinido.
#' @param min,max Limites.
#' @param prefix,suffix Texto visual sin alterar el valor numerico del servidor.
#' @param thousand,decimal Separadores, por defecto Colombia.
#' @param width Ancho CSS.
#' @param ns Namespace del modulo.
#' @return Componente de entrada Shiny para incluir en la UI; su valor está disponible en `input$id`.
#' @param disabled Deshabilitar el control.
#' @param required Marcar el campo como obligatorio.
#' @param help Ayuda opcional del campo.
#' @param error Mensaje de validación opcional.
#' @param readonly Impedir edición por teclado.
#' @export
racafeInputNumeric <- function(id, label, value = 0, format = "number", decimals = NULL,
                               min = NULL, max = NULL, prefix = NULL, suffix = NULL,
                               thousand = ".", decimal = ",", width = "100%", ns = NULL,
                               readonly = FALSE, disabled = FALSE, required = FALSE, help = NULL, error = NULL) {
  presets <- list(
    integer = list(decimals = 0, suffix = NULL), number = list(decimals = 2),
    currency = list(decimals = 2, prefix = "$ "), percent = list(decimals = 2, suffix = "%"),
    factor = list(decimals = 4), kg = list(decimals = 2, suffix = " kg"),
    sacks = list(decimals = 0, suffix = " sacos"), cop = list(decimals = 0, prefix = "$ "),
    usd = list(decimals = 2, prefix = "US$ "))
  cfg <- if (is.list(format)) format else {
    if (!format %in% names(presets)) stop("Formato numerico no reconocido: ", format, call. = FALSE)
    presets[[format]]
  }
  dec <- if (!is.null(decimals)) decimals else if (!is.null(cfg$decimals)) cfg$decimals else 2
  pre <- if (!is.null(prefix)) prefix else cfg$prefix
  suf <- if (!is.null(suffix)) suffix else cfg$suffix
  if (!is.null(pre) && !is.null(suf)) {
    stop("Indique prefijo o sufijo, no ambos (limitacion del wrapper AutoNumeric).", call. = FALSE)
  }
  sym <- if (!is.null(pre)) pre else suf
  pos <- if (!is.null(suf)) "s" else "p"
  tag <- shinyWidgets::autonumericInput(
    inputId = .rf_id(id, ns), label = label, value = value,
    decimalPlaces = as.integer(dec), minimumValue = min, maximumValue = max,
    decimalCharacter = decimal, digitGroupSeparator = thousand,
    currencySymbol = sym, currencySymbolPlacement = pos, width = width
  )
  .rf_input_state(tag, .rf_id(id, ns), readonly, disabled, required, help, error)
}

#' Selector de dia, mes o anio
#' @details El calendario comparte el estilo corporativo con el rango, comienza en lunes
#'   y muestra seis semanas en la vista diaria. Conserva Air Datepicker y su binding Shiny.
#' @param id ID local.
#' @param label Etiqueta.
#' @param value Fecha inicial.
#' @param view day, month, year.
#' @param min,max Fechas limite.
#' @param ns Namespace.
#' @param width Ancho CSS.
#' @return Componente de entrada Shiny para incluir en la UI; su valor está disponible en `input$id`.
#' @param disabled Deshabilitar el control.
#' @param required Marcar el campo como obligatorio.
#' @param help Ayuda opcional del campo.
#' @param error Mensaje de validación opcional.
#' @export
racafeInputDate <- function(id, label = NULL, value = Sys.Date(),
                            view = c("day", "month", "year"), min = NULL, max = NULL,
                            ns = NULL, width = "100%", disabled = FALSE, required = FALSE, help = NULL, error = NULL) {
  view <- match.arg(view)
  value <- as.Date(value)
  if (length(value) != 1L || is.na(value)) stop("value requiere una fecha valida.")
  if (!is.null(min)) min <- as.Date(min)
  if (!is.null(max)) max <- as.Date(max)
  if (!is.null(min) && !is.null(max) && min > max) stop("min debe ser <= max")
  if (!is.null(min) && value < min) value <- min
  if (!is.null(max) && value > max) value <- max
  views <- c(day = "days", month = "months", year = "years")
  formats <- c(day = "dd/MM/yyyy", month = "MM/yyyy", year = "yyyy")
  tag <- shinyWidgets::airDatepickerInput(
    inputId = .rf_id(id, ns), label = label, value = value,
    view = views[[view]], minView = views[[view]], dateFormat = formats[[view]],
    startView = value, minDate = min, maxDate = max, language = "es", width = width,
    addon = "none", firstDay = 1
  )
  tag <- shiny::tagAppendAttributes(tag, class = "rf-date")
  .rf_input_state(tag, .rf_id(id, ns), disabled = disabled, required = required, help = help, error = error)
}

#' Selector de rango de fechas (dos valores Date)
#' @inheritParams racafeInputDate
#' @param start,end Fechas seleccionadas; NULL sin valor inicial.
#' @return Control Shiny cuyo valor en `input$id` es un vector `Date` de longitud dos.
#' @details El separador visual es un guion. El calendario conserva el idioma español
#'   y solo recibe estilos corporativos cuando pertenece a este control. La semana comienza en lunes.
#' @export
racafeInputDateRange <- function(id, label = NULL, start = NULL, end = NULL,
                                 min = NULL, max = NULL, ns = NULL, width = "100%",
                                 disabled = FALSE, required = FALSE, help = NULL, error = NULL) {
  tag <- shiny::dateRangeInput(.rf_id(id, ns), label, start = start, end = end,
                        min = min, max = max, language = "es", format = "dd/mm/yyyy",
                        separator = "-", weekstart = 1, width = width)
  tag <- shiny::tagAppendAttributes(tag, class = "rf-date-range")
  .rf_input_state(tag, .rf_id(id, ns), disabled = disabled, required = required, help = help, error = error)
}

#' Interruptor corporativo
#' @param id ID local.
#' @param label Etiqueta.
#' @param value Estado inicial.
#' @param variant primary, success, warning, danger, neutral.
#' @param ns Namespace.
#' @return Componente de entrada Shiny para incluir en la UI; su valor está disponible en `input$id`.
#' @export
racafeInputSwitch <- function(id, label, value = FALSE,
                              variant = c("primary", "success", "warning", "danger", "neutral"),
                              ns = NULL) {
  variant <- match.arg(variant)
  status <- switch(variant, primary = "danger", success = "success", warning = "warning",
                   danger = "danger", neutral = "secondary")
  shinyWidgets::materialSwitch(.rf_id(id, ns), label = label, value = value,
                               status = status, right = TRUE, width = "100%")
}

#' Opciones radiales corporativas
#' @param id ID local.
#' @param label Etiqueta.
#' @param choices Opciones.
#' @param selected Seleccion inicial.
#' @param inline Disposicion horizontal.
#' @param ns Namespace.
#' @return Componente de entrada Shiny para incluir en la UI; su valor está disponible en `input$id`.
#' @export
racafeInputRadio <- function(id, label, choices, selected = NULL, inline = TRUE, ns = NULL) {
  shiny::radioButtons(.rf_id(id, ns), label, choices = choices, selected = selected,
                      inline = inline, width = "100%")
}

#' Casilla corporativa
#' @param id ID local.
#' @param label Etiqueta.
#' @param value Estado inicial.
#' @param ns Namespace.
#' @return Componente de entrada Shiny para incluir en la UI; su valor está disponible en `input$id`.
#' @export
racafeInputCheckbox <- function(id, label, value = FALSE, ns = NULL) {
  shiny::checkboxInput(.rf_id(id, ns), label, value = value, width = "100%")
}

# Campos informativos ----

#' Campo de solo lectura con copia opcional
#' @param id ID local del campo.
#' @param label Etiqueta.
#' @param value Valor escalar a mostrar como texto; NULL, NA o vacío se muestran en blanco.
#'   La aplicación controla qué información expone.
#' @param copyable Agregar un botón de copia directa.
#' @param multiline Mostrar varias líneas.
#' @param ns Función namespace del módulo.
#' @return Campo readonly, seleccionable y enfocable. La copia informa su resultado
#'   en `input$<id>_copy_copy_result`. Requiere racafeSharedAssets.
#' @export
racafeInputReadOnly <- function(id, label, value = "", copyable = FALSE, multiline = FALSE, ns = NULL) {
  .rf_flag(copyable, "copyable")
  .rf_flag(multiline, "multiline")
  if (is.null(value) || length(value) == 0L) value <- ""
  if (is.atomic(value) && length(value) == 1L) value <- if (is.na(value)) "" else as.character(value)
  .rf_scalar(value, "value", empty = TRUE)
  input <- if (multiline) shiny::textAreaInput(.rf_id(id, ns), label, value, rows = 3, width = "100%") else {
    shiny::textInput(.rf_id(id, ns), label, value, width = "100%")
  }
  input <- htmltools::tagQuery(input)$find(if (multiline) "textarea" else "input")$
    addAttrs(readonly = "readonly")$allTags()
  .rf_attach(shiny::tags$div(class = "rf-readonly", shiny::tags$div(class = "rf-readonly-control", input,
    if (copyable) racafeCopyButton(paste0(id, "_copy"), target = id, ns = ns)),
    shiny::tags$small(class = "rf-readonly-label", "Solo lectura")))
}
