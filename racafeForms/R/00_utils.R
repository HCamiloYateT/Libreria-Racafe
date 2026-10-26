# Helpers internos ----
.rf_id <- function(id, ns = NULL) {
  stopifnot(is.character(id), length(id) == 1L, !is.na(id), nzchar(id))
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
