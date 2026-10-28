# Configuración local ----
#' Configuración corporativa y SMTP en R
#'
#' Lee `config.R` de un directorio local de confianza. Ese archivo define una lista
#' `racafe_mail`; es código R administrado por el operador, nunca contenido de usuarios.
#' Los argumentos explícitos prevalecen sobre el archivo. No se descargan recursos.
#' @param ruta Directorio email de Compartido; por defecto RACAFE_MAIL_HOME.
#' @param logo_url,firma_url URLs HTTPS del logotipo y la imagen de firma.
#' @param remitente Correo del remitente, sin nombre de presentación.
#' @param credenciales Ruta a archivo de credenciales creado por blastula.
#' @param entorno Texto `"true"` o `"false"` para mostrar el aviso de pruebas.
#' @param empresa,web Nombre y URL HTTPS de la empresa.
#' @param color,color_fondo,color_texto Colores CSS hexadecimales de seis dígitos.
#' @param pie Texto del pie corporativo, escapado al renderizar.
#' @param version Versión exacta requerida de email/VERSION, o NULL para aceptar 0.1.x.
#' @return Objeto `racafe_mail_config`, lista validada sin leer credenciales SMTP.
#' @examples
#' \dontrun{
#' config <- racafeMailConfig(ruta = "/home/compartido/email", version = "0.1.0")
#' }
#' @export
racafeMailConfig <- function(ruta = Sys.getenv("RACAFE_MAIL_HOME", "/home/compartido/email"),
                              logo_url = NULL, firma_url = NULL, remitente = NULL, credenciales = NULL,
                              entorno = NULL, empresa = NULL, web = NULL, color = NULL, pie = NULL,
                              color_fondo = NULL, color_texto = NULL, version = NULL) {
  root <- .rmail_root(ruta)
  path <- .rmail_path(root, "config.R")
  env <- new.env(parent = baseenv())
  tryCatch(suppressMessages(suppressWarnings(invisible(utils::capture.output(
    source(path, local = env, encoding = "UTF-8", keep.source = FALSE)
  )))),
           error = function(e) .rmail_stop("No se pudo cargar config.R de correo."))
  if (!exists("racafe_mail", envir = env, inherits = FALSE)) .rmail_stop("config.R debe definir racafe_mail.")
  settings <- .rmail_named(env$racafe_mail, empty = FALSE)
  allowed <- c("empresa", "web", "logo_url", "firma_url", "color", "color_fondo", "color_texto", "pie",
               "remitente", "credenciales", "entorno")
  if (length(setdiff(names(settings), allowed))) .rmail_stop("config.R contiene opciones desconocidas.")
  overrides <- list(logo_url = logo_url, firma_url = firma_url, remitente = remitente, credenciales = credenciales,
                    entorno = entorno, empresa = empresa, web = web, color = color, pie = pie,
                    color_fondo = color_fondo, color_texto = color_texto)
  for (key in names(overrides)) if (!is.null(overrides[[key]])) settings[[key]] <- overrides[[key]]
  if (!is.null(logo_url) && is.null(firma_url)) settings$firma_url <- logo_url
  if (length(setdiff(allowed, names(settings)))) .rmail_stop("Faltan opciones en config.R de correo.")
  settings$ruta <- root
  settings$version <- trimws(.rmail_read(root, "VERSION"))
  if (!grepl("^0\\.1\\.[0-9]+$", settings$version)) .rmail_stop("Version de plantillas no compatible: requiere 0.1.x.")
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
  config$ruta <- .rmail_root(config$ruta)
  for (key in c("logo_url", "firma_url", "web")) config[[key]] <- .rmail_url(config[[key]])
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
  for (name in c("base", "invitacion", "otp", "notificacion", "confirmacion")) {
    .rmail_path(config$ruta, file.path("templates", paste0(name, ".html")))
  }
  .rmail_path(config$ruta, "components/firma.html")
  config
}
