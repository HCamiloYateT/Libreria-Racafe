# Helpers internos ----
.rf_id <- function(id, ns = NULL) {
  stopifnot(is.character(id), length(id) == 1L, !is.na(id), nzchar(id))
  if (!is.null(ns) && !is.function(ns)) stop("'ns' debe ser NULL o una funci\u00f3n.", call. = FALSE)
  if (is.null(ns)) id else ns(id)
}
.rf_choice_values <- function(choices) {
  # Soporta vectores nombrados y listas sin modificar codigos de negocio.
  unname(unlist(choices, use.names = FALSE))
}
.rf_tag <- function(tag, required = FALSE, help = NULL) {
  if (isTRUE(required)) tag <- shiny::tagAppendAttributes(tag, `aria-required` = "true")
  if (is.null(help)) return(tag)
  shiny::tagList(tag, shiny::tags$small(class = "rf-help", help))
}
.rf_type <- function(x, permitted) match.arg(x, permitted)
.rf_button_class <- function(variant, size) {
  paste("rf-btn", paste0("rf-btn--", variant), paste0("rf-btn--", size))
}

# Construcción y validación compartidas ----
.rf_attach <- function(tag) htmltools::attachDependencies(tag, racafeFormsDependencies(), append = TRUE)
.rf_scalar <- function(x, name, empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || (!empty && !nzchar(x))) {
    stop(sprintf("'%s' debe ser una cadena v\u00e1lida.", name), call. = FALSE)
  }
  x
}
.rf_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) stop(sprintf("'%s' debe ser l\u00f3gico.", name), call. = FALSE)
  x
}
.rf_columns <- function(columns) {
  if (!is.numeric(columns) || length(columns) != 1L || is.na(columns) || !columns %in% 1:4) {
    stop("'columns' debe ser un entero de 1 a 4.", call. = FALSE)
  }
  as.integer(columns)
}
.rf_icon <- function(icon) {
  if (is.null(icon) || inherits(icon, "shiny.tag")) return(icon)
  shiny::icon(.rf_scalar(icon, "icon"))
}

# Estado accesible de campos ----
.rf_input_state <- function(tag, id, readonly = FALSE, disabled = FALSE, required = FALSE,
                             help = NULL, error = NULL, kind = "text", min = NULL, max = NULL) {
  .rf_flag(readonly, "readonly")
  .rf_flag(disabled, "disabled")
  .rf_flag(required, "required")
  if (!is.null(error)) .rf_scalar(error, "error", empty = TRUE)
  invalid <- !is.null(error) && nzchar(error)
  described <- c(if (!is.null(help)) paste0(id, "_help"), if (invalid) paste0(id, "_error"))
  for (selector in c("input", "textarea", "select")) {
    tag <- htmltools::tagQuery(tag)$find(selector)$addAttrs(
      readonly = if (readonly) "readonly", disabled = if (disabled) "disabled",
      required = if (required) "required", `aria-required` = if (required) "true",
      `aria-invalid` = if (invalid) "true", `aria-describedby` = if (length(described)) paste(described, collapse = " ")
    )$allTags()
  }
  bound <- function(value) {
    if (is.null(value)) return(NULL)
    if (inherits(value, "POSIXt")) value <- as.numeric(value)
    as.character(value)
  }
  tag <- shiny::tagAppendAttributes(tag, `data-rf-id` = id, `data-rf-kind` = kind,
    `data-rf-required` = tolower(as.character(required)), `data-rf-min` = bound(min), `data-rf-max` = bound(max),
    class = paste("rf-field", if (invalid) "rf-field--error",
                                                      if (disabled) "rf-field--disabled"))
  if (required) tag <- .rf_required_label(tag)
  if (!is.null(help)) tag <- shiny::tagAppendChildren(tag,
    shiny::tags$small(id = paste0(id, "_help"), class = "rf-help", help))
  if (invalid) tag <- shiny::tagAppendChildren(tag,
    shiny::tags$span(id = paste0(id, "_error"), class = "rf-field-error", role = "alert", error))
  .rf_attach(tag)
}

# Títulos de sección sin tamaños heredados de encabezados externos ----
.rf_heading <- function(title, class, level = "h3") {
  normalize <- function(tag) {
    if (inherits(tag, "shiny.tag")) {
      if (tag$name %in% paste0("h", 1:6)) tag$name <- "span"
      tag$children <- lapply(tag$children, normalize)
    } else if (inherits(tag, "html")) {
      tag <- htmltools::HTML(gsub("<(/?)h[1-6]([ >])", "<\\1span\\2", as.character(tag), perl = TRUE))
    } else if (is.list(tag)) tag <- lapply(tag, normalize)
    tag
  }
  shiny::tags[[level]](class = class, normalize(title))
}

# Catálogos con una sola opción vacía ----
.rf_select_choices <- function(choices, unique_choices = FALSE) {
  seen <- character()
  walk <- function(items) {
    keep <- rep(TRUE, length(items))
    for (i in seq_along(items)) {
      if (is.list(items[[i]]) || length(items[[i]]) > 1L) {
        items[[i]] <- walk(items[[i]])
      } else {
        value <- as.character(items[[i]])
        if (length(value) && (unique_choices || identical(value, "")) && value %in% seen) keep[[i]] <- FALSE
        seen <<- c(seen, value)
      }
    }
    items[keep]
  }
  walk(choices)
}

# Asterisco visible sin modificar el texto ni HTML original de la etiqueta ----
.rf_required_label <- function(tag) {
  added <- FALSE
  walk <- function(node) {
    if (!inherits(node, "shiny.tag")) return(node)
    if (node$name == "label" && !added) {
      node$children <- c(node$children, list(shiny::tags$span(class = "rf-required", `aria-hidden` = "true", " *")))
      added <<- TRUE
    } else node$children <- lapply(node$children, walk)
    node
  }
  walk(tag)
}
