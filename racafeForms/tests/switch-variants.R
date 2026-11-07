# Variantes del switch con shinyWidgets real ----
library(racafeForms)
.estados <- c(primary = "danger", success = "success", warning = "warning", danger = "danger", neutral = "default")
for (.variante in names(.estados)) {
  for (.valor in c(TRUE, FALSE)) {
    .html <- htmltools::renderTags(racafeInputSwitch("vista_acumulada", "Vista acumulada",
      value = .valor, variant = .variante, ns = shiny::NS("presupuesto")))$html
    stopifnot(grepl('id="presupuesto-vista_acumulada"', .html, fixed = TRUE),
      grepl(paste0("label-", .estados[[.variante]]), .html, fixed = TRUE),
      identical(grepl('checked="checked"', .html, fixed = TRUE), .valor))
  }
}
stopifnot(inherits(try(racafeInputSwitch("vista", "Vista", variant = "invalida"), silent = TRUE), "try-error"))
