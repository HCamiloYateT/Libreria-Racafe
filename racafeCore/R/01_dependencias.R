# ============================================================
# Seccion 01 — Dependencias
# Carga e instalacion de paquetes de forma conveniente
# ============================================================

#' Cargar paquetes instalando los faltantes
#'
#' Verifica si los paquetes solicitados estan instalados; en caso
#' contrario, intenta instalarlos con dependencias y luego cargarlos.
#'
#' @param pkg Vector de caracteres con nombres de paquetes.
#' @return Vector logico nombrado con el resultado de `library()` por paquete.
#' @export
Loadpkg <- function(pkg) {
  if (!is.character(pkg) || length(pkg) == 0 || any(is.na(pkg)) || any(trimws(pkg) == "")) {
    stop("`pkg` debe ser un vector character no vacio, sin NA ni cadenas vacias.", call. = FALSE)
  }

  nuevos <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    install.packages(nuevos, dependencies = TRUE)
  }

  sapply(pkg, library, character.only = TRUE, logical.return = TRUE)
}


#' Cargar módulos R desde un directorio
#'
#' Busca archivos `.R` dentro de un directorio, los ordena desde las rutas más
#' cercanas a la raíz hacia las más profundas y los carga en el entorno global.
#' Si un archivo falla por depender de objetos definidos en otro módulo, la
#' función reintenta únicamente los archivos pendientes hasta que todos carguen o
#' hasta detectar una pasada sin progreso.
#'
#' @param path Cadena de texto de longitud 1 con la ruta del directorio que
#'   contiene los módulos. Por defecto usa `"misc"`.
#' @param verbose Valor lógico de longitud 1. Si es `TRUE`, imprime el detalle de
#'   los archivos encontrados, su profundidad y los reintentos realizados.
#' @param progress Valor lógico de longitud 1. Si es `TRUE` y `verbose = FALSE`,
#'   muestra una barra de progreso de texto durante la carga.
#'
#' @return Invisiblemente, una lista con tres elementos: `ok`, número de módulos
#'   cargados correctamente; `fallidos`, rutas de módulos que no pudieron
#'   cargarse; y `errores`, lista nombrada con el último mensaje de error por
#'   archivo fallido.
#'
#' @examples
#' dir_modulos <- tempfile("modulos_")
#' dir.create(dir_modulos)
#' writeLines("valor_base <- 2", file.path(dir_modulos, "01_base.R"))
#' writeLines("valor_doble <- valor_base * 2", file.path(dir_modulos, "02_calc.R"))
#' resultado <- load_modules(dir_modulos, progress = FALSE)
#' resultado$ok
#' valor_doble
#'
#' @references
#' Ver `?sys.source` para detalles sobre la carga de scripts R en un entorno.
#'
#' @export
load_modules <- function(path = "misc", verbose = FALSE, progress = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    stop("[load_modules] `path` debe ser una cadena de texto no vacia de longitud 1.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("[load_modules] `verbose` debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("[load_modules] `progress` debe ser TRUE o FALSE.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop(sprintf("[load_modules] El directorio '%s' no existe.", path), call. = FALSE)
  }

  files <- list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(files) > 0L) {
    files <- unique(normalizePath(files, winslash = "/", mustWork = TRUE))
    files <- files[tolower(basename(files)) != "global.r"]
  }

  if (length(files) == 0L) {
    message(sprintf("[WARN] No se encontraron archivos .R en '%s'", path))
    return(invisible(list(ok = 0L, fallidos = character(0), errores = list())))
  }

  depth <- stringr::str_count(files, "/")
  files <- files[order(depth, files)]
  n_total <- length(files)

  if (verbose) {
    message(sprintf("[INFO] %d archivos encontrados en '%s':", n_total, path))
    purrr::walk(files, ~ message(sprintf("  [depth=%d] %s", stringr::str_count(.x, "/"), .x)))
  }

  use_pb <- isTRUE(progress) && !isTRUE(verbose)
  pb <- if (use_pb) txtProgressBar(min = 0L, max = n_total, style = 3, width = 60, char = "=") else NULL
  on.exit(if (!is.null(pb)) close(pb), add = TRUE)

  pendientes <- files
  errores <- list()
  cargados <- character(0)
  pasada <- 1L
  n_procesado <- 0L

  while (length(pendientes) > 0L) {
    fallidos_pasada <- character(0)

    for (f in pendientes) {
      resultado <- tryCatch({
        sys.source(f, envir = globalenv())
        TRUE
      }, error = function(e) conditionMessage(e))

      if (isTRUE(resultado)) {
        cargados <- c(cargados, f)
        errores[[f]] <- NULL
        n_procesado <- n_procesado + 1L
        if (use_pb) {
          setTxtProgressBar(pb, n_procesado)
        } else {
          message(sprintf("  [OK] %s", f))
        }
      } else {
        fallidos_pasada <- c(fallidos_pasada, f)
        errores[[f]] <- resultado
      }
    }

    if (length(fallidos_pasada) == length(pendientes)) {
      if (use_pb) {
        close(pb)
        pb <- NULL
        use_pb <- FALSE
        cat("\n")
      }
      message(sprintf("\n[ERROR] Pasada %d sin progreso. Archivos irresolubles:", pasada))
      purrr::walk(fallidos_pasada, function(f) {
        message(sprintf("  [FAIL] %s\n         -> %s", f, errores[[f]]))
      })
      break
    }

    if (verbose && length(fallidos_pasada) > 0L) {
      message(sprintf(
        "[RETRY] Pasada %d: reintentando %d archivo(s) fallido(s)",
        pasada, length(fallidos_pasada)
      ))
    }

    pendientes <- fallidos_pasada
    pasada <- pasada + 1L
  }

  if (use_pb) {
    close(pb)
    pb <- NULL
    cat("\n")
  }

  fallidos <- names(errores)
  message(sprintf(
    "[DONE] Modulos: %d cargados | %d fallidos de %d totales",
    length(cargados), length(fallidos), n_total
  ))

  invisible(list(ok = length(cargados), fallidos = fallidos, errores = errores))
}
