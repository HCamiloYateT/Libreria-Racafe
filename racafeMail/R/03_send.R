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

#' Enviar explícitamente una plantilla con blastula
#' @param para Un destinatario como dirección simple, sin nombre de presentación.
#' @inheritParams racafeMailRender
#' @details Usa `blastula::smtp_send()` y `blastula::creds_file()`. No imprime contenido,
#'   credenciales ni condiciones del proveedor. Un fallo SMTP produce la clase
#'   `racafe_mail_smtp_error` con mensaje genérico y sin causa adjunta. No reintenta:
#'   un error de red puede ocurrir después de que el servidor acepte el mensaje.
#'   El modo de pruebas agrega un aviso, pero no impide enviar si llama a esta función.
#' @return TRUE invisible si el transporte termina sin error; no acredita entrega al buzón.
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

#' Enviar un código OTP
#' @inheritParams racafeMailEnviar
#' @param codigo Texto de seis dígitos; conserva ceros iniciales.
#' @param nombre Nombre del destinatario; puede estar vacío.
#' @param minutos Vigencia entera positiva, en minutos.
#' @return TRUE invisible al completar el transporte. No genera, almacena ni valida expiración del OTP.
#' @export
racafeMailOTP <- function(para, codigo, nombre = "", minutos = 5L, config = racafeMailConfig()) {
  racafeMailEnviar(para, "otp", list(nombre = nombre, codigo = codigo, vigencia_minutos = minutos), config)
}

#' Enviar una invitación con enlace HTTPS
#' @inheritParams racafeMailEnviar
#' @inheritParams racafeMailOTP
#' @param enlace Enlace HTTPS personal; no se verifica ni consume el token remoto.
#' @param vencimiento Texto legible de vigencia del enlace.
#' @return TRUE invisible al completar el transporte.
#' @export
racafeMailInvitacion <- function(para, nombre, enlace, vencimiento, config = racafeMailConfig()) {
  racafeMailEnviar(para, "invitacion", list(nombre = nombre, enlace = enlace, vencimiento = vencimiento), config)
}

#' Enviar una confirmación genérica
#' @inheritParams racafeMailEnviar
#' @inheritParams racafeMailOTP
#' @param detalle Descripción de la operación, como texto plano.
#' @param fecha Fecha legible de confirmación.
#' @return TRUE invisible al completar el transporte; no registra consentimiento ni firma.
#' @export
racafeMailConfirmacion <- function(para, nombre, detalle, fecha, config = racafeMailConfig()) {
  racafeMailEnviar(para, "confirmacion", list(nombre = nombre, detalle = detalle, fecha = fecha), config)
}

#' Enviar una notificación genérica
#' @inheritParams racafeMailEnviar
#' @inheritParams racafeMailOTP
#' @param mensaje Contenido como texto plano, nunca HTML crudo.
#' @return TRUE invisible al completar el transporte.
#' @export
racafeMailNotificacion <- function(para, nombre, mensaje, config = racafeMailConfig()) {
  racafeMailEnviar(para, "notificacion", list(nombre = nombre, mensaje = mensaje), config)
}
