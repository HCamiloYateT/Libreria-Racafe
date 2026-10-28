# HTML sintético de prueba: no es una copia de las plantillas corporativas ----
.mail_fixture <- function() {
  root <- withr::local_tempdir(.local_envir = testthat::teardown_env())
  dir.create(file.path(root, "templates"))
  dir.create(file.path(root, "components"))
  settings <- list(empresa = "Racafé & Cía.", web = "https://www.racafe.com",
                   logo_url = "https://example.org/logo.png", firma_url = "https://example.org/firma.png",
                   color = "#bf2838", color_fondo = "#f4f4f4", color_texto = "#333333", pie = "Pie corporativo",
                   remitente = "", credenciales = "", entorno = "false")
  cat("racafe_mail <- ", file = file.path(root, "config.R"))
  cat(paste(capture.output(dput(settings)), collapse = "\n"), file = file.path(root, "config.R"), append = TRUE)
  writeLines("0.1.0", file.path(root, "VERSION"))
  templates <- c(base = paste0('<!doctype html><html lang="es"><head><meta charset="utf-8">',
                              '<title>{{asunto}}</title></head><body style="color:{{color_texto}};',
                              'background:{{color_fondo}}"><img src="{{logo_url}}" alt="{{empresa}}">',
                              '<p style="color:{{color}}">{{categoria}} {{preencabezado}}</p>',
                              '[[CONTENIDO_INTERNO]][[FIRMA_INTERNA]]<footer>{{pie}}[[AVISO_PRUEBAS]]</footer>',
                              '</body></html>'),
                 invitacion = '<p>{{nombre}}</p><a href="{{enlace}}">Abrir</a><p>{{vencimiento}}</p>',
                 otp = '<p>{{nombre}}</p><code>{{codigo}}</code><p>{{vigencia_minutos}}</p>',
                 notificacion = '<p>{{nombre}}</p><p>{{mensaje}}</p>',
                 confirmacion = '<p>{{nombre}}</p><p>{{detalle}}</p><p>{{fecha}}</p>')
  for (name in names(templates)) writeLines(templates[[name]], file.path(root, "templates", paste0(name, ".html")))
  writeLines('<a href="{{web}}"><img src="{{firma_url}}" alt="{{empresa}}"></a>',
             file.path(root, "components", "firma.html"))
  root
}
.mail_data <- function() {
  list(invitacion = list(nombre = "Ana", enlace = "https://example.org/?token=ficticio", vencimiento = "Hoy"),
       otp = list(nombre = "Ana", codigo = "012345", vigencia_minutos = 5L),
       notificacion = list(nombre = "Ana", mensaje = "Notificación de prueba"),
       confirmacion = list(nombre = "Ana", detalle = "Solicitud recibida", fecha = "28/10/2026"))
}
.mail_smtp_config <- function(root) {
  credentials <- withr::local_tempfile(.local_envir = testthat::teardown_env())
  writeLines("CREDENCIAL_FICTICIA_NO_REAL", credentials)
  racafeMailConfig(root, remitente = "origen@example.org", credenciales = credentials)
}
