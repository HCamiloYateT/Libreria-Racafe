#!/usr/bin/env Rscript
# Verificar cobertura y estructura de la ayuda desde la raíz del monorepo ----

.paquetes <- list.dirs('.', recursive = FALSE, full.names = TRUE)
.paquetes <- .paquetes[file.exists(file.path(.paquetes, 'DESCRIPTION'))]
if (!length(.paquetes)) stop('Ejecute desde la raíz de Libreria-Racafe.', call. = FALSE)

.etiqueta <- function(x) {
  tag <- attr(x, 'Rd_tag')
  if (is.null(tag)) '' else tag
}

for (.ruta in .paquetes) {
  .paquete <- basename(.ruta)
  .entorno <- new.env(parent = globalenv())
  for (.archivo in sort(list.files(file.path(.ruta, 'R'), '[.]R$', full.names = TRUE))) {
    sys.source(.archivo, envir = .entorno)
  }
  .namespace <- parse(file.path(.ruta, 'NAMESPACE'))
  .exportadas <- unlist(lapply(.namespace, function(x) {
    if (is.call(x) && identical(x[[1]], as.name('export'))) as.character(x[-1])
  }))
  .aliases <- character()
  for (.archivo in list.files(file.path(.ruta, 'man'), '[.]Rd$', full.names = TRUE)) {
    .rd <- tools::parse_Rd(.archivo)
    .problemas <- tools::checkRd(.rd, def_enc = TRUE)
    if (length(.problemas)) stop(paste(.problemas, collapse = '\n'), call. = FALSE)
    .tags <- vapply(.rd, .etiqueta, character(1))
    .temas <- vapply(.rd[.tags == '\\alias'], function(x) paste(unlist(x), collapse = ''), character(1))
    .aliases <- c(.aliases, .temas)
    .argumentos <- .rd[.tags == '\\arguments']
    .items <- if (length(.argumentos)) {
      Filter(function(x) identical(.etiqueta(x), '\\item'), .argumentos[[1]])
    } else list()
    .documentados <- trimws(unlist(strsplit(vapply(.items, function(x) {
      paste(unlist(x[[1]]), collapse = '')
    }, character(1)), ',')))
    for (.tema in .temas) {
      if (!exists(.tema, envir = .entorno, inherits = FALSE) || !is.function(.entorno[[.tema]])) next
      .faltantes <- setdiff(names(formals(.entorno[[.tema]])), .documentados)
      if (length(.faltantes)) {
        stop(sprintf('%s::%s: argumentos sin documentar: %s',
                     .paquete, .tema, paste(.faltantes, collapse = ', ')), call. = FALSE)
      }
    }
    if (any(.temas %in% .exportadas) && !any(.tags == '\\value')) {
      stop(sprintf('%s: falta documentar el retorno en %s.', .paquete, .archivo), call. = FALSE)
    }
    .ejemplo <- tempfile(fileext = '.R')
    writeLines(character(), .ejemplo)
    tools::Rd2ex(.rd, out = .ejemplo)
    parse(.ejemplo)
    unlink(.ejemplo)
  }
  .faltantes <- setdiff(c(.exportadas, .paquete), .aliases)
  if (length(.faltantes)) {
    stop(sprintf('%s: temas sin ayuda: %s', .paquete, paste(.faltantes, collapse = ', ')), call. = FALSE)
  }
  cat(sprintf('%s: ayuda completa para %d funciones públicas y el paquete.\n', .paquete, length(.exportadas)))
}
