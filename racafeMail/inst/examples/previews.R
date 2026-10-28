# Vistas previas con datos ficticios; requiere RACAFE_MAIL_HOME ----
config <- racafeMail::racafeMailConfig(version = "0.1.0", entorno = "true")
ejemplos <- list(
  invitacion = list(nombre = "Ángela", enlace = "https://example.org/autorizar?token=ficticio", vencimiento = "Hoy"),
  otp = list(nombre = "Ángela", codigo = "012345", vigencia_minutos = 5L),
  notificacion = list(nombre = "Ángela", mensaje = "Este es un aviso de prueba."),
  confirmacion = list(nombre = "Ángela", detalle = "Solicitud de prueba recibida", fecha = "28/10/2026")
)
salida <- Sys.getenv("RACAFE_MAIL_PREVIEW_DIR", tempfile("racafe-mail-previews-", tmpdir = getwd()))
dir.create(salida, mode = "0700")
for (plantilla in names(ejemplos)) {
  racafeMail::racafeMailPreview(plantilla, ejemplos[[plantilla]], file.path(salida, paste0(plantilla, ".html")), config)
}
# La ruta no contiene datos del destinatario; no se imprime el contenido del correo.
message("Vistas previas de prueba: ", salida)
