#' Correos corporativos autocontenidos
#'
#' Configuración R, firma, cuatro plantillas y logotipo incluidos en racafeMail.
#' Render y preview funcionan sin SMTP, carpetas externas ni descargas.
#' @name racafeMail
#' @rdname racafeMail
#' @aliases racafeMail-package
#' @param ruta NULL usa recursos internos. Una ruta explícita permite compatibilidad con email/ de 0.1.x.
#'   No se consulta RACAFE_MAIL_HOME automáticamente. La configuración externa es R de confianza.
#' @param logo_url,firma_url Imagen PNG local/embebida o URL HTTPS; NULL usa la imagen incluida.
#'   Un logo explícito se usa también como firma salvo firma_url explícita.
#' @param remitente Dirección simple de remitente. NULL usa RACAFE_MAIL_FROM o FIRMA_BLASTULA_FROM.
#' @param credenciales Ruta externa a credenciales de blastula; NULL usa RACAFE_MAIL_CRED o FIRMA_BLASTULA_CRED.
#' @param entorno Texto true/false; NULL usa RACAFE_MAIL_TEST_MODE o FIRMA_TEST_MODE. Solo agrega aviso de pruebas.
#' @param empresa,web Nombre y URL HTTPS de la empresa; NULL conserva los valores incluidos.
#' @param color,color_fondo,color_texto Colores hexadecimales de seis dígitos; NULL conserva los incluidos.
#' @param pie Texto del pie, escapado al renderizar; NULL conserva el incluido.
#' @param version Versión exacta requerida; NULL acepta la instalada o el contrato externo compatible.
#' @param config Objeto creado por racafeMailConfig().
#' @param plantilla invitacion, otp, notificacion o confirmacion.
#' @param datos Lista nombrada de escalares no NA. Solo nombre admite texto vacío; no se acepta HTML crudo.
#' @param archivo Archivo de preview. No debe reemplazar recursos del paquete ni credenciales.
#' @param overwrite Permitir reemplazar un preview existente, FALSE por defecto.
#' @param para Una dirección de destinatario, sin listas ni cabeceras.
#' @param codigo Texto de seis dígitos; conserva ceros iniciales.
#' @param nombre Nombre del destinatario; puede estar vacío.
#' @param minutos Vigencia entera positiva del OTP, en minutos; no verifica expiración real.
#' @param enlace URL HTTPS personal; no consume ni verifica el token remoto.
#' @param vencimiento Texto legible de vigencia del enlace.
#' @param detalle Descripción de confirmación, como texto plano.
#' @param fecha Fecha legible de confirmación.
#' @param mensaje Notificación como texto plano.
#' @return Config devuelve una lista racafe_mail_config; Firma devuelve HTML de la firma;
#'   Render devuelve list(asunto, html); Preview devuelve una ruta invisible.
#'   Enviar y los cuatro atajos devuelven TRUE invisible al completar el transporte;
#'   no acreditan entrega al buzón ni registran firmas/consentimientos.
#' @details Las cuatro plantillas requieren, respectivamente:
#'   invitacion (nombre, enlace, vencimiento), otp (nombre, codigo, vigencia_minutos),
#'   notificacion (nombre, mensaje), confirmacion (nombre, detalle, fecha).
#'   Los valores se escapan en una pasada y solo la base inserta parciales internos.
#'   Variables desconocidas, duplicadas, faltantes y marcadores sin resolver se rechazan.
#'
#'   El logo se incluye como data URI en previews; blastula lo convierte en partes CID
#'   al componer el mensaje SMTP. No se descarga la imagen. Las URL HTTPS personalizadas
#'   son opcionales y las consulta el cliente al mostrar el correo. La apariencia/entrega
#'   en Gmail/Outlook requiere validación con buzones reales; no se garantiza por el render.
#'
#'   Los envíos son explícitos, usan smtp_send y creds_file y no reintentan automáticamente.
#'   Los errores del proveedor se sustituyen por racafe_mail_smtp_error sin datos sensibles.
#'   El modo de pruebas agrega aviso, pero no bloquea una llamada explícita al envío.
#'   Los previews pueden contener OTP/enlaces: no publicarlos ni imprimirlos en logs.
#' @examples
#' config <- racafeMailConfig()
#' correo <- racafeMailRender("otp", list(nombre = "Ana", codigo = "012345", vigencia_minutos = 5), config)
#' stopifnot(grepl("012345", correo$html, fixed = TRUE))
NULL

# Identidad incluida: una imagen para logo y firma, sin descargas ----
.rmail_image <- function(x) {
  x <- .rmail_string(x)
  if (startsWith(x, "https://")) return(.rmail_url(x))
  if (!startsWith(x, "data:") && file.exists(x) && !dir.exists(x)) {
    x <- tryCatch(base64enc::dataURI(file = x, mime = "image/png"),
                  error = function(e) .rmail_stop("No se pudo leer la imagen corporativa."))
  }
  if (!grepl("^data:image/png;base64,[A-Za-z0-9+/]+={0,2}$", x)) {
    .rmail_stop("La imagen requiere HTTPS, un archivo PNG o una imagen PNG embebida valida.")
  }
  bytes <- tryCatch(suppressWarnings(base64enc::base64decode(sub("^data:image/png;base64,", "", x))),
                    error = function(e) raw())
  signature <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
  if (length(bytes) < 8L || !identical(bytes[seq_len(8L)], signature)) .rmail_stop("Imagen PNG no valida.")
  x
}
.rmail_defaults <- function() {
  logo <- system.file("img", "logo.png", package = "racafeMail")
  if (!nzchar(logo)) .rmail_stop("La instalacion de racafeMail no contiene el logotipo.")
  image <- .rmail_image(logo)
  list(empresa = "Racaf\u00e9 & C\u00eda. S.C.A.", web = "https://www.racafe.com",
       logo_url = image, firma_url = image, color = "#bf2838", color_fondo = "#f4f4f4", color_texto = "#333333",
       pie = "Este es un mensaje autom\u00e1tico. Por favor, no responda a este correo.",
       remitente = Sys.getenv("RACAFE_MAIL_FROM", Sys.getenv("FIRMA_BLASTULA_FROM", "")),
       credenciales = Sys.getenv("RACAFE_MAIL_CRED", Sys.getenv("FIRMA_BLASTULA_CRED", "")),
       entorno = Sys.getenv("RACAFE_MAIL_TEST_MODE", Sys.getenv("FIRMA_TEST_MODE", "false")))
}

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
  if (is.null(config$ruta)) return(.rmail_templates[[nombre]])
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

# Configuración local ----
#' @rdname racafeMail
#' @export
racafeMailConfig <- function(ruta = NULL,
                              logo_url = NULL, firma_url = NULL, remitente = NULL, credenciales = NULL,
                              entorno = NULL, empresa = NULL, web = NULL, color = NULL, pie = NULL,
                              color_fondo = NULL, color_texto = NULL, version = NULL) {
  root <- if (is.null(ruta)) NULL else .rmail_root(ruta)
  settings <- if (is.null(root)) .rmail_defaults() else {
    path <- .rmail_path(root, "config.R")
    env <- new.env(parent = baseenv())
    tryCatch(suppressMessages(suppressWarnings(invisible(utils::capture.output(
      source(path, local = env, encoding = "UTF-8", keep.source = FALSE)
    )))), error = function(e) .rmail_stop("No se pudo cargar config.R de correo."))
    if (!exists("racafe_mail", envir = env, inherits = FALSE)) .rmail_stop("config.R debe definir racafe_mail.")
    .rmail_named(env$racafe_mail, empty = FALSE)
  }
  allowed <- c("empresa", "web", "logo_url", "firma_url", "color", "color_fondo", "color_texto", "pie",
               "remitente", "credenciales", "entorno")
  if (length(setdiff(names(settings), allowed))) .rmail_stop("config.R contiene opciones desconocidas.")
  overrides <- list(logo_url = logo_url, firma_url = firma_url, remitente = remitente, credenciales = credenciales,
                    entorno = entorno, empresa = empresa, web = web, color = color, pie = pie,
                    color_fondo = color_fondo, color_texto = color_texto)
  for (key in names(overrides)) if (!is.null(overrides[[key]])) settings[[key]] <- overrides[[key]]
  if (!is.null(logo_url) && is.null(firma_url)) settings$firma_url <- logo_url
  if (length(setdiff(allowed, names(settings)))) .rmail_stop("Faltan opciones en config.R de correo.")
  settings["ruta"] <- list(root)
  settings$version <- if (is.null(root)) as.character(utils::packageVersion("racafeMail")) else {
    trimws(.rmail_read(root, "VERSION"))
  }
  if (!grepl("^0\\.[12]\\.[0-9]+$", settings$version)) {
    .rmail_stop("Version de plantillas no compatible: requiere 0.1.x o 0.2.x.")
  }
  if (!is.null(version) && !identical(.rmail_string(version), settings$version)) {
    .rmail_stop("La version local no coincide con la solicitada.")
  }
  settings$entorno <- tolower(.rmail_string(settings$entorno))
  if (!settings$entorno %in% c("true", "false")) .rmail_stop("entorno debe ser true o false.")
  settings$pruebas <- identical(settings$entorno, "true")
  class(settings) <- "racafe_mail_config"
  .rmail_config(settings)
}
.rmail_config <- function(config) {
  if (!inherits(config, "racafe_mail_config")) .rmail_stop("Use racafeMailConfig para crear la configuracion.")
  if (!is.null(config$ruta)) config$ruta <- .rmail_root(config$ruta)
  config$web <- .rmail_url(config$web)
  for (key in c("logo_url", "firma_url")) config[[key]] <- .rmail_image(config[[key]])
  for (key in c("color", "color_fondo", "color_texto")) {
    if (!grepl("^#[A-Fa-f0-9]{6}$", .rmail_string(config[[key]]))) .rmail_stop("Color hexadecimal no valido.")
  }
  for (key in c("empresa", "pie")) config[[key]] <- .rmail_string(config[[key]])
  config$remitente <- .rmail_string(config$remitente, empty = TRUE)
  if (nzchar(config$remitente)) config$remitente <- .rmail_email(config$remitente)
  config$credenciales <- .rmail_string(config$credenciales, empty = TRUE)
  if (!is.logical(config$pruebas) || length(config$pruebas) != 1L || is.na(config$pruebas)) {
    .rmail_stop("Indicador de pruebas no valido.")
  }
  if (is.null(config$ruta)) return(config)
  for (name in c("base", "invitacion", "otp", "notificacion", "confirmacion")) {
    .rmail_path(config$ruta, file.path("templates", paste0(name, ".html")))
  }
  .rmail_path(config$ruta, "components/firma.html")
  config
}

# Renderizado sin SMTP ----
#' @rdname racafeMail
#' @export
racafeMailFirma <- function(config = racafeMailConfig()) {
  config <- .rmail_config(config)
  .rmail_fill(.rmail_archivo(config, "components", "firma"), .rmail_values(.rmail_brand(config)))
}

#' @rdname racafeMail
#' @export
racafeMailRender <- function(plantilla = c("invitacion", "otp", "notificacion", "confirmacion"),
                             datos = list(), config = racafeMailConfig()) {
  if (missing(plantilla)) plantilla <- "invitacion"
  if (!is.character(plantilla) || length(plantilla) != 1L || is.na(plantilla) ||
      !plantilla %in% c("invitacion", "otp", "notificacion", "confirmacion")) .rmail_stop("Plantilla no permitida.")
  config <- .rmail_config(config)
  datos <- .rmail_named(datos)
  required <- switch(plantilla, invitacion = c("nombre", "enlace", "vencimiento"),
                     otp = c("nombre", "codigo", "vigencia_minutos"), notificacion = c("nombre", "mensaje"),
                     confirmacion = c("nombre", "detalle", "fecha"))
  if (length(setdiff(required, names(datos)))) .rmail_stop("Faltan variables requeridas para la plantilla.")
  if (length(setdiff(names(datos), required))) .rmail_stop("Variables adicionales no permitidas para la plantilla.")
  if (plantilla == "otp" && !is.character(datos$codigo)) .rmail_stop("El codigo OTP debe ser texto de seis digitos.")
  for (key in required) datos[[key]] <- .rmail_scalar(datos[[key]], empty = key == "nombre")
  if (plantilla == "otp") {
    # Exigir texto conserva los ceros iniciales y evita conversiones silenciosas.
    if (!grepl("^[0-9]{6}$", datos$codigo)) .rmail_stop("El codigo OTP debe contener seis digitos.")
    minutes <- datos$vigencia_minutos
    if (!grepl("^[1-9][0-9]*$", minutes) || !is.finite(as.numeric(minutes))) .rmail_stop("Vigencia no valida.")
  }
  if (plantilla == "invitacion") datos$enlace <- .rmail_url(datos$enlace)
  texts <- switch(plantilla,
    invitacion = c("Racaf\u00e9 | Autorizaci\u00f3n para el tratamiento de datos personales",
                   "Autorizaci\u00f3n de datos personales"),
    otp = c("Racaf\u00e9 | C\u00f3digo de verificaci\u00f3n de correo electr\u00f3nico", "Verificaci\u00f3n de correo"),
    notificacion = c("Racaf\u00e9 | Notificaci\u00f3n", "Notificaci\u00f3n"),
    confirmacion = c("Racaf\u00e9 | Confirmaci\u00f3n de solicitud", "Confirmaci\u00f3n"))
  # El cuerpo y la firma se rellenan una vez; la base los recibe como fragmentos confiables.
  values <- .rmail_values(c(datos, .rmail_brand(config)))
  body <- .rmail_fill(.rmail_archivo(config, "templates", plantilla), values)
  headers <- list(asunto = texts[[1]], preencabezado = texts[[2]], categoria = texts[[2]])
  values <- .rmail_values(c(headers, .rmail_brand(config)))
  values[["[[CONTENIDO_INTERNO]]"]] <- body
  values[["[[FIRMA_INTERNA]]"]] <- racafeMailFirma(config)
  values[["[[AVISO_PRUEBAS]]"]] <- if (config$pruebas) {
    '<p style="font-family:Arial,sans-serif;font-size:11px;color:#777;">Este proceso est\u00e1 en pruebas.</p>'
  } else ""
  html <- .rmail_fill(.rmail_archivo(config, "templates", "base"), values)
  list(asunto = texts[[1]], html = enc2utf8(html))
}

#' @rdname racafeMail
#' @export
racafeMailPreview <- function(plantilla, datos, archivo, config = racafeMailConfig(), overwrite = FALSE) {
  config <- .rmail_config(config)
  archivo <- .rmail_string(archivo)
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) .rmail_stop("overwrite no valido.")
  parent <- dirname(archivo)
  if (!dir.exists(parent)) .rmail_stop("No existe el directorio de salida de la vista previa.")
  target <- file.path(normalizePath(parent, winslash = "/", mustWork = TRUE), basename(archivo))
  resolved <- normalizePath(target, winslash = "/", mustWork = FALSE)
  credential <- if (nzchar(config$credenciales)) {
    normalizePath(config$credenciales, winslash = "/", mustWork = FALSE)
  } else ""
  roots <- c(system.file(package = "racafeMail"), config$ruta)
  protected <- any(vapply(roots, function(root) {
    startsWith(target, paste0(root, "/")) || startsWith(resolved, paste0(root, "/"))
  }, logical(1)))
  if (protected || identical(resolved, credential)) {
    .rmail_stop("La vista previa no puede reemplazar plantillas ni credenciales.")
  }
  if (dir.exists(target) || (file.exists(target) && !overwrite)) .rmail_stop("La salida ya existe.")
  content <- racafeMailRender(plantilla, datos, config)
  tmp <- tempfile(pattern = ".racafe-mail-", tmpdir = parent, fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch({
    if (!file.create(tmp, showWarnings = FALSE)) .rmail_stop("No se pudo crear la vista previa.")
    Sys.chmod(tmp, mode = "0600")
    writeLines(enc2utf8(content$html), tmp, useBytes = TRUE)
    if (!file.rename(tmp, target)) .rmail_stop("No se pudo guardar la vista previa.")
  }, error = function(e) .rmail_stop("No se pudo guardar la vista previa."))
  invisible(normalizePath(target, winslash = "/", mustWork = TRUE))
}

# Transporte explícito ----
.rmail_passthrough <- function(title, html_header, html_body, html_footer, ...) html_body
.rmail_compose <- function(html) {
  blastula::compose_email(body = htmltools::HTML(html), template = .rmail_passthrough)
}
.rmail_transport <- function(content, para, config) {
  correo <- .rmail_compose(content$html)
  blastula::smtp_send(email = correo, from = config$remitente, to = para, subject = content$asunto,
                      credentials = blastula::creds_file(file = config$credenciales), verbose = FALSE)
  invisible(TRUE)
}

#' @rdname racafeMail
#' @export
racafeMailEnviar <- function(para, plantilla, datos, config = racafeMailConfig()) {
  para <- .rmail_email(para)
  config <- .rmail_config(config)
  if (!nzchar(config$remitente) || !nzchar(config$credenciales) ||
      !file.exists(config$credenciales) || dir.exists(config$credenciales) ||
      file.access(config$credenciales, 4L) != 0L) .rmail_stop("Credenciales SMTP o remitente no configurados.")
  content <- racafeMailRender(plantilla, datos, config)
  tryCatch({
    # Capturar cualquier salida accidental del proveedor; nunca reenviar su condición.
    invisible(utils::capture.output(suppressMessages(withCallingHandlers(
      .rmail_transport(content, para, config), warning = function(w) invokeRestart("muffleWarning")
    ))))
  }, error = function(e) {
    stop(structure(list(message = "No se pudo completar el envio SMTP.", call = NULL),
                   class = c("racafe_mail_smtp_error", "error", "condition")))
  })
  invisible(TRUE)
}

#' @rdname racafeMail
#' @export
racafeMailOTP <- function(para, codigo, nombre = "", minutos = 5L, config = racafeMailConfig()) {
  racafeMailEnviar(para, "otp", list(nombre = nombre, codigo = codigo, vigencia_minutos = minutos), config)
}

#' @rdname racafeMail
#' @export
racafeMailInvitacion <- function(para, nombre, enlace, vencimiento, config = racafeMailConfig()) {
  racafeMailEnviar(para, "invitacion", list(nombre = nombre, enlace = enlace, vencimiento = vencimiento), config)
}

#' @rdname racafeMail
#' @export
racafeMailConfirmacion <- function(para, nombre, detalle, fecha, config = racafeMailConfig()) {
  racafeMailEnviar(para, "confirmacion", list(nombre = nombre, detalle = detalle, fecha = fecha), config)
}

#' @rdname racafeMail
#' @export
racafeMailNotificacion <- function(para, nombre, mensaje, config = racafeMailConfig()) {
  racafeMailEnviar(para, "notificacion", list(nombre = nombre, mensaje = mensaje), config)
}
