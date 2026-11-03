# Esquema de campos y reglas de servidor ----
.rf_text_content <- function(tag) {
  if (inherits(tag, "shiny.tag") && identical(tag$attribs$class, "rf-required")) return("")
  if (inherits(tag, "shiny.tag")) return(paste(vapply(tag$children, .rf_text_content, character(1)), collapse = " "))
  if (is.list(tag)) return(paste(vapply(tag, .rf_text_content, character(1)), collapse = " "))
  if (is.character(tag)) return(trimws(gsub("<[^>]+>", "", paste(tag, collapse = " "))))
  ""
}
.rf_field_label <- function(tag) {
  if (inherits(tag, "shiny.tag") && identical(tag$name, "label")) return(trimws(.rf_text_content(tag)))
  children <- if (inherits(tag, "shiny.tag")) tag$children else if (is.list(tag)) tag else list()
  for (child in children) {
    label <- .rf_field_label(child)
    if (!is.null(label)) return(label)
  }
  NULL
}
.rf_form_schema <- function(form, ns) {
  if (is.function(form)) form <- form()
  fields <- list(); form_id <- NULL
  walk <- function(tag) {
    if (inherits(tag, "shiny.tag")) {
      attrs <- tag$attribs
      if (grepl("(^| )rf-form( |$)", attrs$class %||% "") && !is.null(attrs$id)) form_id <<- attrs$id
      if (!is.null(attrs$`data-rf-id`)) {
        fields[[length(fields) + 1L]] <<- list(id = attrs$`data-rf-id`, label = .rf_field_label(tag),
          required = identical(attrs$`data-rf-required`, "true"), kind = attrs$`data-rf-kind`,
          min = attrs$`data-rf-min`, max = attrs$`data-rf-max`)
        return(invisible(NULL))
      }
      lapply(tag$children, walk)
    } else if (is.list(tag)) lapply(tag, walk)
    invisible(NULL)
  }
  # El mismo validador admite esquemas declarativos para controles dinámicos/no corporativos.
  if (is.list(form) && !inherits(form, c("shiny.tag", "shiny.tag.list")) &&
      length(form) && !is.null(names(form)) && all(vapply(form, is.list, logical(1)))) {
    fields <- lapply(names(form), function(id) {
      utils::modifyList(list(id = id, required = FALSE, kind = "text"), form[[id]])
    })
  } else walk(form)
  if (!length(fields)) stop("El formulario no contiene campos registrados.", call. = FALSE)
  prefix <- if (is.null(ns)) "" else ns("")
  for (i in seq_along(fields)) {
    id <- .rf_scalar(fields[[i]]$id, "id")
    kind <- fields[[i]]$kind %||% "text"
    if (!is.character(kind) || length(kind) != 1L || is.na(kind) ||
        !kind %in% c("text", "select", "numeric", "logical", "date", "range", "datetime")) {
      stop("kind no es un tipo de campo v\u00e1lido.", call. = FALSE)
    }
    local <- if (nzchar(prefix) && startsWith(id, prefix)) substring(id, nchar(prefix) + 1L) else id
    fields[[i]]$local <- local
    fields[[i]]$id <- if (is.null(ns) || startsWith(id, prefix)) id else ns(id)
    if (is.null(fields[[i]]$label) || !nzchar(fields[[i]]$label)) fields[[i]]$label <- local
  }
  names(fields) <- vapply(fields, `[[`, character(1), "local")
  if (anyDuplicated(names(fields))) stop("Hay IDs de campo duplicados.", call. = FALSE)
  list(fields = fields, id = form_id)
}
`%||%` <- function(x, y) if (is.null(x)) y else x
.rf_empty_value <- function(value) {
  is.null(value) || !length(value) || (is.data.frame(value) && !nrow(value)) || anyNA(value) ||
    (is.character(value) && all(!nzchar(trimws(value, whitespace = "[\\h\\v]"))))
}

#' Validar un formulario en el servidor
#' @param input reactiveValues de Shiny o lista de valores locales.
#' @param form UI generada con racafeForm o función que devuelve la UI actual. También
#'   admite lista nombrada de esquemas con label, required, kind, min y max.
#' @param rules Lista nombrada, o función que devuelve la lista actual, con reglas
#'   por campo function(value, values) que devuelven
#'   TRUE o un mensaje; cada campo puede tener una lista de funciones.
#' @param required Lista nombrada (o función que la devuelve) de lógicos o funciones
#'   function(values) para obligatoriedad condicional.
#' @param ns Namespace; por defecto el de la sesión reactiva actual, si existe.
#' @return Resultado rf_validation: valid, errors, pending, labels, ids, values, form_id y required.
#' @details Valida datos en R, no atributos HTML. FALSE y 0 son valores válidos; para
#'   exigir aceptar una casilla declare una regla isTRUE. kind numeric/date/range/datetime
#'   comprueba tipos, finitud y límites; range exige dos fechas ordenadas.
#'   Los campos opcionales vacíos omiten reglas. Use required para condiciones de negocio.
#' @export
racafeValidateForm <- function(input, form, rules = list(), required = list(),
                               ns = if (!is.null(shiny::getDefaultReactiveDomain()))
                                 shiny::getDefaultReactiveDomain()$ns else NULL) {
  if (!is.null(ns) && !is.function(ns)) stop("ns debe ser una funci\u00f3n.", call. = FALSE)
  if (is.function(rules)) rules <- rules()
  if (is.function(required)) required <- required()
  schema <- .rf_form_schema(form, ns)
  fields <- schema$fields
  for (config in list(rules, required)) {
    if (!is.list(config) || (length(config) && (is.null(names(config)) || any(!nzchar(names(config))) ||
        any(!names(config) %in% names(fields))))) stop("Las reglas deben nombrar campos del formulario.")
  }
  values <- lapply(names(fields), function(id) input[[id]])
  names(values) <- names(fields)
  errors <- character()
  mandatory_fields <- stats::setNames(rep(FALSE, length(fields)), names(fields))
  for (id in names(fields)) {
    field <- fields[[id]]; value <- values[[id]]
    mandatory <- if (id %in% names(required)) required[[id]] else field$required %||% FALSE
    if (is.function(mandatory)) mandatory <- mandatory(values)
    .rf_flag(mandatory, paste0("required$", id))
    mandatory_fields[[id]] <- mandatory
    empty <- .rf_empty_value(value)
    if (empty) {
      if (mandatory) errors[[id]] <- "Complete este campo."
      next
    }
    kind <- field$kind %||% "text"
    if (kind == "logical" && (!is.logical(value) || length(value) != 1L)) {
      errors[[id]] <- "Seleccione un estado l\u00f3gico v\u00e1lido."
    } else if (kind == "text" && (!is.character(value) || length(value) != 1L)) {
      errors[[id]] <- "Ingrese un texto v\u00e1lido."
    } else if (kind == "select" && !is.atomic(value)) {
      errors[[id]] <- "Seleccione una opci\u00f3n v\u00e1lida."
    } else if (kind == "numeric" && (!is.numeric(value) || length(value) != 1L || any(!is.finite(value)))) {
      errors[[id]] <- "Ingrese un n\u00famero v\u00e1lido."
    } else if (kind %in% c("date", "range", "datetime")) {
      expected <- if (kind == "datetime") inherits(value, "POSIXct") else inherits(value, "Date")
      count <- if (kind == "range") 2L else 1L
      if (!expected || length(value) != count || any(!is.finite(as.numeric(value)))) {
        errors[[id]] <- "Ingrese una fecha v\u00e1lida."
      } else if (kind == "range" && value[[1]] > value[[2]]) errors[[id]] <- "Revise el orden de las fechas."
    }
    if (!id %in% names(errors) && kind %in% c("numeric", "date", "range", "datetime")) {
      parse <- switch(kind, numeric = as.numeric, date = as.Date, range = as.Date,
                      datetime = function(x) as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"))
      if (!is.null(field$min) && any(as.numeric(value) < as.numeric(parse(field$min)))) {
        errors[[id]] <- "Valor inferior al m\u00ednimo."
      }
      if (!is.null(field$max) && any(as.numeric(value) > as.numeric(parse(field$max)))) {
        errors[[id]] <- "Valor superior al m\u00e1ximo."
      }
    }
    if (!id %in% names(errors) && id %in% names(rules)) {
      functions <- rules[[id]]
      if (is.function(functions)) functions <- list(functions)
      for (rule in functions) {
        if (!is.function(rule)) stop("Cada regla debe ser una funci\u00f3n.", call. = FALSE)
        result <- rule(value, values)
        if (!isTRUE(result)) {
          errors[[id]] <- if (is.character(result) && length(result) == 1L && !is.na(result) &&
            nzchar(trimws(result))) result else "Revise este campo."
          break
        }
      }
    }
  }
  structure(list(valid = !length(errors), errors = errors, pending = names(errors), values = values,
    labels = vapply(fields, `[[`, character(1), "label"), ids = vapply(fields, `[[`, character(1), "id"),
    form_id = schema$id, required = mandatory_fields), class = "rf_validation")
}
.rf_validation_send <- function(session, result, inline, focus) {
  session$sendCustomMessage("racafe-validation", list(form = result$form_id, fields = unname(as.list(result$ids)),
    required = lapply(names(result$ids), function(id) {
      list(id = result$ids[[id]], required = isTRUE(result$required[[id]]))
    }),
    errors = lapply(result$pending, function(id) list(id = result$ids[[id]], label = result$labels[[id]],
      text = as.vector(result$errors[[id]], mode = "character"))), inline = inline, focus = focus))
}

#' Mostrar o limpiar el resultado de validación
#' @param session Sesión Shiny.
#' @param result Resultado de racafeValidateForm.
#' @param mode Se aceptan inline, alert y both por compatibilidad; todos muestran solo errores por campo, sin alertas.
#' @param focus Llevar el foco al primer pendiente, incluso dentro de details.
#' @return Resultado recibido, invisiblemente.
#' @export
racafeShowValidation <- function(session, result, mode = c("inline", "alert", "both"), focus = TRUE) {
  mode <- match.arg(mode)
  if (!inherits(result, "rf_validation")) stop("result debe proceder de racafeValidateForm.")
  .rf_flag(focus, "focus")
  .rf_validation_send(session, result, TRUE, focus)
  invisible(result)
}

#' Vincular validación diferida y limpieza automática
#' @inheritParams racafeValidateForm
#' @inheritParams racafeShowValidation
#' @param initial Mostrar validación inline antes de enviar; FALSE por defecto.
#' @return Objeto con validate() (lógico, muestra errores), check() (resultado sin mostrar),
#'   result() y attempted() reactivos. Regístrelo una sola vez por formulario/sesión.
#' @details Tras el primer validate, revalida al editar y limpia mensajes, sin repetir
#'   alertas ni mover el foco. Las acciones deben comprobar validate antes de negocio.
#'   Una sesión conserva una instancia por formulario; llamadas repetidas devuelven esa instancia.
#'   destroy libera su registro; registre una instancia nueva si cambia su configuración.
#' @export
racafeFormValidation <- function(input, form, session = shiny::getDefaultReactiveDomain(), rules = list(),
                                 required = list(), mode = c("inline", "alert", "both"), initial = FALSE) {
  mode <- match.arg(mode)
  .rf_flag(initial, "initial")
  if (is.null(session)) stop("Se requiere una sesi\u00f3n Shiny activa.")
  schema <- shiny::isolate(.rf_form_schema(form, session$ns))
  key <- schema$id %||% paste(vapply(schema$fields, `[[`, character(1), "id"), collapse = "|")
  if (is.null(session$userData$rf_validators)) session$userData$rf_validators <- new.env(parent = emptyenv())
  cache <- session$userData$rf_validators
  if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache))
  attempted <- shiny::reactiveVal(initial)
  result <- shiny::reactiveVal(NULL)
  check <- function() racafeValidateForm(input, form, rules, required, ns = session$ns)
  observer <- shiny::observe({
    current <- check()
    if (!attempted()) {
      .rf_validation_send(session, current, FALSE, FALSE)
      return()
    }
    result(current)
    .rf_validation_send(session, current, TRUE, FALSE)
  }, domain = session)
  validate <- function(show = TRUE) {
    .rf_flag(show, "show")
    current <- shiny::isolate(check())
    attempted(TRUE); result(current)
    if (show) racafeShowValidation(session, current, mode)
    current$valid
  }
  object <- list(validate = validate, check = function() shiny::isolate(check()), result = shiny::reactive(result()),
    attempted = shiny::reactive(attempted()), destroy = function() {
      observer$destroy()
      if (exists(key, envir = cache, inherits = FALSE)) rm(list = key, envir = cache)
    })
  assign(key, object, envir = cache)
  object
}
