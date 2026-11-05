# CONSTRUCCION DE LAS UI MIGRADAS SIN EJECUTAR SERVIDORES DEL CRM ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Uso: Rscript check_migration_ui.R directorio_migrado")
library(shiny)
library(bs4Dash)
env <- new.env(parent = globalenv())
env$Choices <- function() list(personas = c("Ana", "Luis"))
env$`%||%` <- function(x, y) if (is.null(x)) y else x
for (name in c("00_header.R", "01_tabla_indicadores.R", "02_notificaciones_tareas.R",
               "03_form_persona.R", "04_migrar_cartera.R")) {
  expressions <- parse(file.path(args[[1]], name))
  for (expression in expressions) {
    if (is.call(expression) && identical(expression[[1]], as.name("<-")) && length(expression) == 3L &&
        is.call(expression[[3]]) && identical(expression[[3]][[1]], as.name("function"))) eval(expression, env)
  }
}
uis <- list(env$TablaIndicadoresUI("ind"), env$NotificacionesTareasUI("tasks"), env$IndFormularioUI("person"),
  env$MigrarCarteraUI(shiny::NS("migration")),
  env$.header_items_clientes(data.frame(CLCliNit = "900", CLLinNegNo = "Nacional", LinNegCod = "A",
    PerRazSoc = "Cliente <script>texto</script>"), "nuevo", "select_client"),
  env$.header_tabla(data.frame(Cliente = "Café", Valor = 1234.56)))
for (ui in uis) {
  html <- htmltools::renderTags(ui)$html
  stopifnot(nzchar(html))
}
sys.source(file.path(args[[1]], "header.R"), envir = env)
stopifnot(is.list(env$header) && length(env$header) > 0L)
html <- htmltools::renderTags(env$header)$html
stopifnot(!grepl("li_menu_", html, fixed = TRUE), !grepl("crmHeaderRetenerMenu", html, fixed = TRUE))
cat("7 construcciones de UI migradas correctas; ningún módulo de servidor ni operación BD fue ejecutado.\n")
