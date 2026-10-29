# Archivos nativos ----
.rf_file_options <- function(accept, max_size) {
  if (!is.null(accept)) {
    if (!is.character(accept) || anyNA(accept)) stop("accept debe ser texto sin NA.", call. = FALSE)
    accept <- trimws(unlist(strsplit(accept, ",", fixed = TRUE)))
    valid <- grepl("^[.][A-Za-z0-9_.+-]+$|^[A-Za-z0-9!#$&^_.+-]+/([A-Za-z0-9!#$&^_.+-]+|[*])$", accept)
    if (any(!valid)) stop("accept requiere extensiones o tipos MIME validos.", call. = FALSE)
  }
  if (!is.null(max_size) && (!is.numeric(max_size) || length(max_size) != 1L || is.na(max_size) ||
      !is.finite(max_size) || max_size < 1 || max_size %% 1 != 0)) {
    stop("max_size debe ser un numero entero positivo de bytes.", call. = FALSE)
  }
  list(accept = accept, max_size = max_size)
}
.rf_file_matches <- function(name, type, accept) {
  if (!length(accept)) return(TRUE)
  any(vapply(tolower(accept), function(rule) {
    if (startsWith(rule, ".")) return(endsWith(tolower(name), rule))
    if (endsWith(rule, "/*")) return(startsWith(tolower(type), substr(rule, 1L, nchar(rule) - 1L)))
    identical(tolower(type), rule)
  }, logical(1)))
}

#' Selector corporativo de archivos con binding nativo de Shiny
#' @param id ID local del control.
#' @param label Etiqueta del campo.
#' @param accept Extensiones (p. ej. .xlsx, .csv, .pdf) o tipos MIME; NULL no restringe la sugerencia.
#' @param multiple Permitir varios archivos.
#' @param max_size Limite por archivo en bytes; NULL sin limite adicional al de Shiny.
#' @param help Ayuda opcional del campo.
#' @param placeholder Texto sin archivo seleccionado.
#' @param size Densidad compact o normal.
#' @param disabled Deshabilitar seleccion e interaccion.
#' @param ns Namespace del modulo, aplicado una sola vez.
#' @return fileInput nativo: input$id recibe name, size, type y datapath.
#' @details Los controles visuales son orientativos y no sustituyen validacion del servidor.
#'   Use racafeValidateInputFile y un lector especifico del formato antes de procesar contenido.
#'   El MIME y nombre declarados no acreditan contenido seguro. La subida sigue el protocolo
#'   nativo de Shiny, incluso si se muestra un error visual; valide siempre antes de actuar.
#'   max_size no cambia shiny.maxRequestSize. No se pueden asignar archivos locales desde R;
#'   para reiniciar, reconstruya el control con renderUI y limpie el estado asociado del servidor.
#' @export
racafeInputFile <- function(id, label = "Seleccionar archivo", accept = NULL, multiple = FALSE,
                            max_size = NULL, help = NULL, placeholder = "Ning\u00fan archivo seleccionado",
                            size = c("compact", "normal"), disabled = FALSE, ns = NULL) {
  size <- match.arg(size)
  .rf_flag(multiple, "multiple")
  .rf_flag(disabled, "disabled")
  options <- .rf_file_options(accept, max_size)
  id <- .rf_id(id, ns)
  tag <- shiny::fileInput(id, label, multiple = multiple, accept = options$accept, width = "100%",
    buttonLabel = shiny::tagList(shiny::icon("file"), shiny::tags$span("Seleccionar")), placeholder = placeholder)
  described <- paste(c(if (!is.null(help)) paste0(id, "_help"), paste0(id, "_files"), paste0(id, "_error")),
                     collapse = " ")
  tag <- htmltools::tagQuery(tag)$find(".shiny-input-file")$addAttrs(disabled = if (disabled) "disabled",
    `aria-describedby` = described, `aria-invalid` = "false", `aria-label` = label,
    `data-rf-max-size` = options$max_size)$allTags()
  tag <- htmltools::tagQuery(tag)$find(".form-control")$addAttrs(
    disabled = if (disabled) "disabled", `aria-label` = "Archivo seleccionado", tabindex = "-1")$allTags()
  tag <- shiny::tagAppendAttributes(tag, class = paste("rf-field rf-file", paste0("rf-file--", size),
                                                       if (disabled) "rf-field--disabled"))
  if (!is.null(help)) tag <- shiny::tagAppendChildren(tag,
    shiny::tags$small(id = paste0(id, "_help"), class = "rf-help", help))
  tag <- shiny::tagAppendChildren(tag,
    shiny::tags$ul(id = paste0(id, "_files"), class = "rf-file-list", `aria-live` = "polite"),
    shiny::tags$span(id = paste0(id, "_error"), class = "rf-file-error", role = "alert"))
  .rf_attach(tag)
}

#' Validar archivos recibidos antes de leer su contenido
#' @param files Valor de input$id producido por fileInput; NULL se considera sin seleccion.
#' @inheritParams racafeInputFile
#' @return Lista con valid (logico) y errors (textos). No modifica ni procesa los archivos.
#' @details Comprueba metadatos, extension/MIME declarados, tamano real, vacio y lectura
#'   del archivo temporal. El contenido debe validarse con el parser apropiado del consumidor.
#' @export
racafeValidateInputFile <- function(files, accept = NULL, max_size = NULL) {
  options <- .rf_file_options(accept, max_size)
  errors <- character()
  if (is.null(files) || (is.data.frame(files) && nrow(files) == 0L)) {
    return(list(valid = FALSE, errors = "Seleccione al menos un archivo."))
  }
  if (!is.data.frame(files) || !all(c("name", "size", "type", "datapath") %in% names(files)) ||
      !is.character(files$name) || !is.character(files$type) ||
      !is.character(files$datapath) || !is.numeric(files$size)) {
    return(list(valid = FALSE, errors = "Metadatos de archivo no validos."))
  }
  for (i in seq_len(nrow(files))) {
    name <- files$name[[i]]
    type <- files$type[[i]]
    path <- files$datapath[[i]]
    bytes <- files$size[[i]]
    if (!is.character(name) || is.na(name) || !nzchar(name) || !is.character(type) || is.na(type) ||
        !is.character(path) || is.na(path) || !is.numeric(bytes) || is.na(bytes) || !is.finite(bytes) || bytes < 0) {
      errors <- c(errors, "Metadatos de archivo no validos.")
      next
    }
    if (!.rf_file_matches(name, type, options$accept)) errors <- c(errors, "Extension o tipo no permitido.")
    if (bytes == 0) errors <- c(errors, "El archivo esta vacio.")
    if (!is.null(max_size) && bytes > max_size) errors <- c(errors, "El archivo supera el tamano maximo.")
    readable <- tryCatch({
      info <- file.info(path)
      if (is.na(info$size) || isTRUE(info$isdir) || file.access(path, 4L) != 0L) stop("lectura")
      if (info$size != bytes) errors <- c(errors, "El tamano real no coincide con el declarado.")
      if (info$size == 0 && bytes != 0) errors <- c(errors, "El archivo esta vacio.")
      if (!is.null(max_size) && info$size > max_size) errors <- c(errors, "El archivo supera el tamano maximo.")
      if (info$size > 0 && length(suppressWarnings(readBin(path, "raw", n = 1L))) != 1L) stop("lectura")
      TRUE
    }, error = function(e) FALSE)
    if (!readable) errors <- c(errors, "No se pudo leer el archivo temporal.")
  }
  list(valid = !length(errors), errors = unique(errors))
}

# Notificaciones nativas ----
.rf_notify_id <- function(session, id) {
  prefix <- session$ns("rf-notify-")
  if (is.null(id)) {
    count <- session$userData$.rf_notify_count
    count <- if (is.null(count)) 1L else count + 1L
    session$userData$.rf_notify_count <- count
    return(paste0(prefix, "auto-", count))
  }
  .rf_scalar(id, "id")
  if (!grepl("^[A-Za-z0-9_.:-]+$", id)) stop("id contiene caracteres no permitidos.", call. = FALSE)
  result <- if (startsWith(id, prefix)) id else paste0(prefix, "key-", id)
  if (!grepl("^[A-Za-z0-9_.:-]+$", result)) stop("Namespace de notificacion no valido.", call. = FALSE)
  result
}

#' Notificacion corporativa informativa, no bloqueante
#' @param session Sesion Shiny, incluida la de un modulo.
#' @param title Titulo de texto plano, escapado.
#' @param text Descripcion opcional de texto plano, escapada.
#' @param type info, success, warning o error.
#' @param duration Segundos antes del cierre; NULL mantiene visible hasta retirarla.
#' @param id ID local opcional, o el ID devuelto para actualizar la misma notificacion.
#' @param close_button Permitir cierre manual.
#' @return ID real de la notificacion, para reutilizar o pasar a racafeRemoveNotify.
#' @details Usa showNotification con type default, sin SweetAlert. Solo las notificaciones
#'   rf- se colocan arriba a la derecha, bajo el navbar; se ocultan mientras hay un modal
#'   o sidebar movil abierto. Se muestran hasta tres propias; las anteriores quedan en cola
#'   visual hasta que haya espacio, conservando sus temporizadores nativos.
#'   Los IDs locales se prefijan y reciben namespace una vez para no reemplazar mensajes ajenos.
#' @export
racafeNotify <- function(session, title, text = NULL, type = c("info", "success", "warning", "error"),
                         duration = 5, id = NULL, close_button = TRUE) {
  type <- match.arg(type)
  .rf_flag(close_button, "close_button")
  title <- as.vector(.rf_scalar(title, "title"), mode = "character")
  if (!is.null(text)) text <- as.vector(.rf_scalar(text, "text", empty = TRUE), mode = "character")
  if (!is.null(duration) && (!is.numeric(duration) || length(duration) != 1L || is.na(duration) ||
      !is.finite(duration) || duration <= 0)) stop("duration debe ser NULL o segundos positivos.", call. = FALSE)
  id <- .rf_notify_id(session, id)
  icons <- c(info = "circle-info", success = "circle-check",
             warning = "triangle-exclamation", error = "circle-xmark")
  ui <- .rf_attach(shiny::tags$div(class = paste("rf-notify", paste0("rf-notify--", type)),
    `data-rf-type` = type, role = if (type %in% c("warning", "error")) "alert" else "status",
    shiny::icon(icons[[type]]), shiny::tags$div(class = "rf-notify-content",
      shiny::tags$strong(class = "rf-notify-title", title),
      if (!is.null(text)) shiny::tags$p(class = "rf-notify-text", text))))
  shiny::showNotification(ui, duration = duration, closeButton = close_button, id = id,
                          type = "default", session = session)
}

#' Retirar una notificacion corporativa por su identificador real
#' @param id ID devuelto por racafeNotify, sin volver a aplicar namespace.
#' @param session Sesion opcional; por defecto el dominio reactivo actual.
#' @return ID retirado, segun removeNotification nativo de Shiny.
#' @export
racafeRemoveNotify <- function(id, session = shiny::getDefaultReactiveDomain()) {
  shiny::removeNotification(.rf_scalar(id, "id"), session = session)
}
