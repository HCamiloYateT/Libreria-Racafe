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
                             help = NULL, error = NULL) {
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
  tag <- shiny::tagAppendAttributes(tag, class = paste("rf-field", if (invalid) "rf-field--error",
                                                      if (disabled) "rf-field--disabled"))
  if (!is.null(help)) tag <- shiny::tagAppendChildren(tag,
    shiny::tags$small(id = paste0(id, "_help"), class = "rf-help", help))
  if (invalid) tag <- shiny::tagAppendChildren(tag,
    shiny::tags$span(id = paste0(id, "_error"), class = "rf-field-error", role = "alert", error))
  .rf_attach(tag)
}
