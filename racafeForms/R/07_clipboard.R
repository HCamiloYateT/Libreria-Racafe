# Portapapeles ----

#' Botón de copia con resultado real comunicado a Shiny
#' @inheritParams racafeButton
#' @param text Texto estático; admite cadena vacía. Proporcione solo una de text, target o image.
#' @param target ID local de un elemento cuyo valor o texto se copiará.
#' @param image URL local, HTTPS o data URI de una imagen PNG.
#' @param notify Mostrar notificación de éxito o fallo después de finalizar la operación.
#' @return Botón corporativo. El resultado llega a `input$<id>_copy_result` como lista
#'   con success, type, error y request_id, sin incluir el contenido copiado.
#' @details Requiere racafeSharedAssets en la UI y un navegador compatible en HTTPS o localhost.
#'   Los permisos pueden impedir la copia. No se informa éxito hasta completar la promesa del navegador.
#' @export
racafeCopyButton <- function(id, label = "Copiar", text = NULL, target = NULL, image = NULL,
                              ns = NULL, notify = TRUE, variant = "secondary", size = "sm") {
  if (sum(!vapply(list(text, target, image), is.null, logical(1))) != 1L) {
    stop("Proporcione exactamente uno de text, target o image.", call. = FALSE)
  }
  .rf_flag(notify, "notify")
  if (!is.null(text)) .rf_scalar(text, "text", empty = TRUE)
  if (!is.null(image)) .rf_scalar(image, "image")
  button <- racafeButton(id, label, ns = ns, variant = variant, size = size)
  .rf_attach(shiny::tagAppendAttributes(button, class = "rf-copy-button",
    `data-rf-copy-text` = text, `data-rf-copy-target` = if (!is.null(target)) .rf_id(target, ns),
    `data-rf-copy-image` = image, `data-rf-notify` = if (notify) "true" else "false"))
}

#' Solicitar copia de texto desde el servidor
#' @param session Sesión Shiny del módulo que recibirá el resultado.
#' @param id ID local para el resultado; se recibe en `input$<id>_copy_result`.
#' @param text Texto que la aplicación ha decidido exponer al navegador.
#' @param notify Mostrar una notificación después del resultado real.
#' @return Invisiblemente NULL; el éxito o fallo se comunica de forma asíncrona al input de resultado.
#' @details Requiere racafeSharedAssets en la UI. Inicie la copia como respuesta a una acción del usuario.
#'   El navegador puede exigir permisos o activación reciente. Nunca envíe secretos que no deban estar en la UI.
#' @export
racafeCopyText <- function(session, id, text, notify = TRUE) {
  .rf_scalar(text, "text", empty = TRUE)
  .rf_flag(notify, "notify")
  session$sendCustomMessage("rf-copy-text", list(text = text, notify = notify,
    result_id = session$ns(paste0(.rf_id(id), "_copy_result"))))
  invisible(NULL)
}

#' Solicitar copia de una imagen PNG desde el servidor
#' @inheritParams racafeCopyText
#' @param image URL de una imagen PNG accesible al navegador, o data URI PNG.
#' @return Invisiblemente NULL; el resultado asíncrono se comunica al input de resultado.
#' @details Requiere ClipboardItem y navigator.clipboard.write. Una URL externa debe permitir CORS.
#'   No acepta rutas del sistema de archivos. No convierte otros formatos a PNG.
#' @export
racafeCopyImage <- function(session, id, image, notify = TRUE) {
  .rf_scalar(image, "image")
  .rf_flag(notify, "notify")
  session$sendCustomMessage("rf-copy-image", list(image = image, notify = notify,
    result_id = session$ns(paste0(.rf_id(id), "_copy_result"))))
  invisible(NULL)
}
