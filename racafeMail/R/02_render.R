# Renderizado sin SMTP ----
#' Firma corporativa compartida
#' @param config Configuración creada con [racafeMailConfig()].
#' @return Texto HTML de la firma, con variables escapadas y sin envío.
#' @export
racafeMailFirma <- function(config = racafeMailConfig()) {
  config <- .rmail_config(config)
  .rmail_fill(.rmail_archivo(config, "components", "firma"), .rmail_values(.rmail_brand(config)))
}

#' Renderizar una de las cuatro plantillas locales
#'
#' @param plantilla Una de `invitacion`, `otp`, `notificacion` o `confirmacion`.
#' @param datos Lista nombrada con los valores de la plantilla; no admite HTML crudo.
#' @inheritParams racafeMailFirma
#' @details Variables requeridas: invitacion (`nombre`, `enlace`, `vencimiento`),
#'   otp (`nombre`, `codigo`, `vigencia_minutos`), notificacion (`nombre`, `mensaje`)
#'   y confirmacion (`nombre`, `detalle`, `fecha`). Solo `nombre` admite texto vacío.
#'   El código OTP es texto de seis dígitos y la vigencia un entero positivo.
#'   El enlace debe usar HTTPS. No se verifica ni consume el token remoto.
#'   Solo la base puede insertar parciales internos; las variables se escapan en una pasada.
#' @return Lista con `asunto` y `html` UTF-8. No accede a SMTP ni descarga imágenes.
#' @examples
#' \dontrun{
#' config <- racafeMailConfig()
#' correo <- racafeMailRender("otp", list(nombre = "Ana", codigo = "012345", vigencia_minutos = 5), config)
#' }
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

#' Guardar una vista previa local sin enviar correo
#' @inheritParams racafeMailRender
#' @param archivo Ruta HTML de salida. Puede contener OTP o enlaces; protéjala y elimínela después de revisar.
#' @param overwrite Permitir reemplazar un archivo existente; FALSE por defecto.
#' @return Ruta absoluta del archivo creado, de forma invisible. No abre navegador ni llama a SMTP.
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
  if (startsWith(target, paste0(config$ruta, "/")) || startsWith(resolved, paste0(config$ruta, "/")) ||
      identical(resolved, credential)) {
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
