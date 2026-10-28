# Validación privada ----
.rmail_stop <- function(message) stop(message, call. = FALSE)
.rmail_scalar <- function(x, empty = FALSE) {
  if (!is.atomic(x) || length(x) != 1L || is.na(x) || !is.null(dim(x))) {
    .rmail_stop("Cada valor debe ser un escalar no NA.")
  }
  value <- enc2utf8(as.character(x))
  if (!empty && !nzchar(trimws(value))) .rmail_stop("Se requiere un valor no vacio.")
  value
}
.rmail_string <- function(x, empty = FALSE) {
  if (!is.character(x)) .rmail_stop("Se requiere un texto escalar.")
  .rmail_scalar(x, empty)
}
.rmail_named <- function(x, empty = TRUE) {
  if (!is.list(x) || (!empty && !length(x)) ||
      (length(x) && (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))))) {
    .rmail_stop("Se requiere una lista con nombres unicos.")
  }
  x
}
.rmail_url <- function(x) {
  x <- .rmail_string(x)
  if (!grepl("^https://[A-Za-z0-9.-]+(:[0-9]{1,5})?([/?#].*)?$", x) ||
      grepl("[[:space:]<>\\\\]", x)) .rmail_stop("Se requiere una URL HTTPS sin credenciales.")
  x
}
.rmail_email <- function(x) {
  x <- .rmail_string(x)
  if (!grepl("^[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", x)) {
    .rmail_stop("Direccion de correo no valida.")
  }
  x
}
.rmail_root <- function(ruta) {
  ruta <- .rmail_string(ruta)
  if (!dir.exists(ruta)) .rmail_stop("No existe el directorio local de correo.")
  normalizePath(ruta, winslash = "/", mustWork = TRUE)
}
.rmail_path <- function(root, relative) {
  path <- file.path(root, relative)
  if (!file.exists(path) || dir.exists(path) || file.access(path, 4L) != 0L) {
    .rmail_stop("Falta un archivo local de correo requerido o no es legible.")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!startsWith(path, paste0(root, "/"))) .rmail_stop("Archivo fuera del directorio de correo.")
  path
}
.rmail_read <- function(root, relative) {
  tryCatch(paste(readLines(.rmail_path(root, relative), encoding = "UTF-8", warn = FALSE), collapse = "\n"),
           error = function(e) .rmail_stop("No se pudo leer un archivo local de correo permitido."))
}
.rmail_archivo <- function(config, tipo, nombre) {
  allowed <- list(templates = c("base", "invitacion", "otp", "notificacion", "confirmacion"), components = "firma")
  if (!tipo %in% names(allowed) || !nombre %in% allowed[[tipo]]) .rmail_stop("Plantilla no permitida.")
  .rmail_read(config$ruta, file.path(tipo, paste0(nombre, ".html")))
}
.rmail_escape <- function(x) as.character(htmltools::htmlEscape(.rmail_scalar(x, empty = TRUE), attribute = TRUE))

# Sustitución de una sola pasada: los datos nunca se vuelven a interpretar como marcadores ----
.rmail_fill <- function(template, values) {
  pattern <- "\\{\\{[^{}]*\\}\\}|\\[\\[[^][]*\\]\\]"
  matches <- gregexpr(pattern, template, perl = TRUE)
  tokens <- regmatches(template, matches)[[1]]
  replacements <- vapply(tokens, function(token) {
    if (!token %in% names(values)) .rmail_stop("Variables sin resolver en la plantilla HTML.")
    values[[token]]
  }, character(1))
  remaining <- regmatches(template, matches, invert = TRUE)[[1]]
  if (any(grepl("{{", remaining, fixed = TRUE)) || any(grepl("[[", remaining, fixed = TRUE))) {
    .rmail_stop("Marcador incompleto en la plantilla HTML.")
  }
  regmatches(template, matches) <- list(replacements)
  template
}
.rmail_values <- function(values) stats::setNames(lapply(values, .rmail_escape), paste0("{{", names(values), "}}"))

.rmail_brand <- function(config) {
  config[c("empresa", "web", "logo_url", "firma_url", "color", "color_fondo", "color_texto", "pie")]
}
