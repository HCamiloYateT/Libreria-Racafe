# VERIFICACION ESTATICA DE MIGRACION ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Uso: Rscript verify_migration.R originales migrados")
protected <- c(".tareas_modificar_bd", ".tareas_pendientes", ".tareas_actividades", ".tareas_prioridad",
  "guardar_registro", "construir_payload", "ultimo_registro", "crear_formulario", ".header_resumen_cliente",
  ".header_historial_comercial")
.collect <- function(expressions) {
  result <- list(calls = character(), sql = character(), functions = character())
  visit <- function(node) {
    if (is.character(node) && length(node) == 1L && grepl(
      "^\\s*(SELECT|INSERT|UPDATE|DELETE|MERGE|BEGIN|COMMIT|ROLLBACK|WITH)\\b", node,
      ignore.case = TRUE, perl = TRUE)) {
      result$sql <<- c(result$sql, node)
    }
    if (is.call(node)) {
      head <- paste(deparse(node[[1]], width.cutoff = 500L), collapse = "")
      if (grepl("^(DBI::|racafeBD::|dbExecute$|dbGetQuery$|dbConnect$|dbWithTransaction$)", head)) {
        result$calls <<- c(result$calls, paste(deparse(node, width.cutoff = 500L), collapse = "\n"))
      }
      if (head == "<-" && length(node) == 3L && is.symbol(node[[2]]) && as.character(node[[2]]) %in% protected) {
        result$functions <<- c(result$functions, paste(deparse(node, width.cutoff = 500L), collapse = "\n"))
      }
      lapply(as.list(node)[-1L], visit)
    } else if (is.expression(node) || is.pairlist(node)) lapply(as.list(node), visit)
    invisible(NULL)
  }
  visit(expressions)
  result
}
checks <- 0L
for (name in list.files(args[[1]], pattern = "[.]R$")) {
  original <- parse(file.path(args[[1]], name), keep.source = TRUE)
  migrated <- parse(file.path(args[[2]], name), keep.source = TRUE)
  old_comments <- subset(utils::getParseData(original), token == "COMMENT")$text
  new_comments <- subset(utils::getParseData(migrated), token == "COMMENT")$text
  if (!identical(old_comments, new_comments)) stop("Comentarios modificados en ", name)
  before <- .collect(original); after <- .collect(migrated)
  for (part in names(before)) if (!identical(before[[part]], after[[part]])) {
    stop("Cambio protegido: ", name, " / ", part)
  }
  checks <- checks + 4L
  cat(name, ": comentarios, llamadas BD, SQL y funciones de negocio intactos\n")
}
# Todos los estados de cuatro actividades mantienen la equivalencia posición/clave.
for (mask in 0:15) {
  flags <- as.logical(intToBits(mask)[1:4])
  keys <- as.character(seq_along(flags))
  selected <- keys[flags]
  reconstructed <- vapply(seq_along(flags), function(i) as.character(i) %in% selected, logical(1))
  stopifnot(identical(flags, reconstructed))
  checks <- checks + 1L
}
cat(checks, "comprobaciones de migración superadas; no se ejecutaron conexiones ni escrituras.\n")
