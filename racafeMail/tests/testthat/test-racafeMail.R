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


test_that("configuración R usa archivo, overrides, entorno y versión sin credenciales", {
  root <- .mail_fixture()
  cfg <- racafeMailConfig(root, version = "0.1.0")
  expect_s3_class(cfg, "racafe_mail_config")
  expect_identical(cfg$color, "#bf2838")
  expect_identical(cfg$version, "0.1.0")
  expect_false(cfg$pruebas)
  withr::local_envvar(RACAFE_MAIL_HOME = root)
  expect_null(racafeMailConfig()$ruta)
  cfg <- racafeMailConfig(root, logo_url = "https://example.org/new.png", color = "#DA291C",
                         color_fondo = "#eeeeee", color_texto = "#111111", empresa = "Otra & Cía", entorno = "TRUE")
  expect_identical(cfg$firma_url, cfg$logo_url)
  expect_true(cfg$pruebas)
  expect_identical(cfg$empresa, "Otra & Cía")
  expect_error(racafeMailConfig(root, version = "0.2.0"), "version")
  expect_error(racafeMailConfig(root, color = "red;display:none"), "Color")
  expect_error(racafeMailConfig(root, color_fondo = "red"), "Color")
  expect_error(racafeMailConfig(root, entorno = "quizas"), "entorno")
  for (url in c("javascript:alert(1)", "http://example.org", "cid:imagen", "img/logo.png",
                "https://user:pass@example.org", "https://example.org\nsecret")) {
    expect_error(racafeMailConfig(root, logo_url = url), "HTTPS")
    expect_error(racafeMailConfig(root, firma_url = url), "HTTPS")
  }
  expect_error(racafeMailConfig(root, remitente = "a@b.org\r\nBcc: x@example.org"), "correo")
})

test_that("rutas locales y symlinks no permiten traversal", {
  root <- .mail_fixture()
  expect_error(racafeMailConfig(file.path(root, "ausente")), "directorio")
  cfg <- racafeMailConfig(root)
  for (name in c("../otp", "/tmp/otp", "otp.html", "OTP", NA_character_)) {
    expect_error(racafeMailRender(name, .mail_data()$otp, cfg), "Plantilla")
  }
  outside <- withr::local_tempfile()
  writeLines("CONTENIDO_PRIVADO", outside)
  target <- file.path(root, "templates", "otp.html")
  unlink(target)
  expect_error(racafeMailConfig(root), "archivo")
  if (.Platform$OS.type == "unix") {
    expect_true(file.symlink(outside, target))
    expect_error(racafeMailRender("otp", .mail_data()$otp, cfg), "fuera")
    unlink(file.path(root, "config.R"))
    expect_true(file.symlink(outside, file.path(root, "config.R")))
    expect_error(racafeMailConfig(root), "fuera")
  }
})

test_that("configuración incompleta, con errores y versiones incompatibles se rechaza", {
  root <- .mail_fixture()
  writeLines('stop("SECRETO_CONFIG")', file.path(root, "config.R"))
  expect_error(racafeMailConfig(root), "^No se pudo cargar config.R de correo\\.$")
  writeLines('otra <- list()', file.path(root, "config.R"))
  expect_error(racafeMailConfig(root), "racafe_mail")
  writeLines('racafe_mail <- list(empresa = "X")', file.path(root, "config.R"))
  expect_error(racafeMailConfig(root), "Faltan")
  writeLines('racafe_mail <- list(empresa = "X", desconocido = "Y")', file.path(root, "config.R"))
  expect_error(racafeMailConfig(root), "desconocidas")
  root2 <- .mail_fixture()
  writeLines("1.0.0", file.path(root2, "VERSION"))
  expect_error(racafeMailConfig(root2), "compatible")
  expect_error(racafeMailRender("otp", .mail_data()$otp, config = list()), "racafeMailConfig")
})


test_that("las cuatro plantillas usan la misma firma y se renderizan sin SMTP", {
  cfg <- racafeMailConfig(.mail_fixture())
  local_mocked_bindings(.rmail_transport = function(...) stop("SMTP NO DEBE LLAMARSE"))
  firma <- racafeMailFirma(cfg)
  for (name in names(.mail_data())) {
    mail <- racafeMailRender(name, .mail_data()[[name]], cfg)
    expect_named(mail, c("asunto", "html"))
    expect_match(mail$html, firma, fixed = TRUE)
    expect_false(grepl("{{", mail$html, fixed = TRUE))
    expect_false(grepl("[[", mail$html, fixed = TRUE))
    expect_match(mail$asunto, "Racafé", fixed = TRUE)
  }
  expect_match(racafeMailRender("otp", .mail_data()$otp, cfg)$html, "012345", fixed = TRUE)
  expect_false(grepl("Este proceso", racafeMailRender("otp", .mail_data()$otp, cfg)$html, fixed = TRUE))
  cfg$pruebas <- TRUE
  expect_match(racafeMailRender("otp", .mail_data()$otp, cfg)$html, "Este proceso", fixed = TRUE)
})

test_that("HTML, atributos, Unicode y marcadores del usuario se escapan sin reinterpretación", {
  cfg <- racafeMailConfig(.mail_fixture())
  datos <- .mail_data()$notificacion
  datos$nombre <- 'Ángela Ñandú ☕ & <script> "test"'
  datos$mensaje <- '{{nombre}} [[FIRMA_INTERNA]] <img src=x onerror="alert(1)"> \\1 $1'
  mail <- racafeMailRender("notificacion", datos, cfg)
  expect_match(mail$html, "Ángela Ñandú ☕ &amp; &lt;script&gt; &quot;test&quot;", fixed = TRUE)
  expect_match(mail$html, '{{nombre}} [[FIRMA_INTERNA]] &lt;img', fixed = TRUE)
  expect_match(mail$html, '\\1 $1', fixed = TRUE)
  expect_false(grepl('<script>', mail$html, fixed = TRUE))
  datos <- .mail_data()$invitacion
  datos$enlace <- 'https://example.org/?a="x"&b=2'
  expect_match(racafeMailRender("invitacion", datos, cfg)$html, 'a=&quot;x&quot;&amp;b=2', fixed = TRUE)
  cfg$empresa <- 'Empresa " onclick="robo & <x>'
  expect_match(racafeMailFirma(cfg), '&quot; onclick=&quot;robo &amp; &lt;x&gt;', fixed = TRUE)
  cfg$logo_url <- "javascript:alert(1)"
  expect_error(racafeMailFirma(cfg), "HTTPS")
})

test_that("datos faltantes, extra, no escalares o vacíos se rechazan sin mostrar valores", {
  cfg <- racafeMailConfig(.mail_fixture())
  expect_error(racafeMailRender("otp", list(), cfg), "Faltan")
  expect_error(racafeMailRender("otp", unname(.mail_data()$otp), cfg), "nombres")
  expect_error(racafeMailRender("otp", c(.mail_data()$otp, list(codigo = "123456")), cfg), "nombres")
  expect_error(racafeMailRender("otp", c(.mail_data()$otp, list(secreto = "123456")), cfg), "adicionales")
  for (bad in list(NULL, NA_character_, character(), c("a", "b"), list("x"), "", " ")) {
    d <- .mail_data()$notificacion
    d["mensaje"] <- list(bad)
    expect_error(racafeMailRender("notificacion", d, cfg))
  }
  for (code in list("12345", "1234567", "abcdef", "12345\n", 123456, NA_character_, c("123456", "654321"))) {
    d <- .mail_data()$otp
    d$codigo <- code
    expect_error(racafeMailRender("otp", d, cfg))
  }
  for (minutes in c("0", "-1", "1.5", "1e2", "Inf", "abc")) {
    d <- .mail_data()$otp
    d$vigencia_minutos <- minutes
    expect_error(racafeMailRender("otp", d, cfg), "Vigencia")
  }
  for (link in c("javascript:alert(1)", "http://example.org/?token=SECRETO", "https://a.org\nBcc: x")) {
    d <- .mail_data()$invitacion
    d$enlace <- link
    expect_error(racafeMailRender("invitacion", d, cfg), "HTTPS")
  }
})

test_that("plantillas con marcadores desconocidos o incompletos no se envían", {
  root <- .mail_fixture()
  cfg <- racafeMailConfig(root)
  for (html in c("{{desconocido}}", "[[CONTENIDO_INTERNO]]", "{{nombre", "[[FIRMA")) {
    writeLines(html, file.path(root, "templates", "notificacion.html"))
    expect_error(racafeMailRender("notificacion", .mail_data()$notificacion, cfg), "[Vv]ariable|Marcador")
  }
})

test_that("vista previa no requiere SMTP y protege archivos existentes", {
  root <- .mail_fixture()
  cfg <- racafeMailConfig(root)
  local_mocked_bindings(.rmail_transport = function(...) stop("NO ENVIAR"))
  file <- withr::local_tempfile(fileext = ".html")
  expect_identical(racafeMailPreview("otp", .mail_data()$otp, file, cfg), normalizePath(file))
  expect_match(paste(readLines(file, encoding = "UTF-8"), collapse = "\n"), "012345")
  expect_error(racafeMailPreview("otp", .mail_data()$otp, file, cfg), "ya existe")
  expect_identical(racafeMailPreview("otp", .mail_data()$otp, file, cfg, overwrite = TRUE), normalizePath(file))
  expect_error(racafeMailPreview("otp", .mail_data()$otp, file.path(root, "templates", "otp.html"), cfg,
                                 overwrite = TRUE), "reemplazar")
  if (.Platform$OS.type == "unix") expect_equal(as.integer(file.info(file)$mode), strtoi("600", base = 8L))
})


test_that("preview protege credenciales y archivos enlazados dentro de los recursos", {
  root <- .mail_fixture()
  cfg <- .mail_smtp_config(root)
  expect_error(racafeMailPreview("otp", .mail_data()$otp, cfg$credenciales, cfg, overwrite = TRUE), "reemplazar")
  if (.Platform$OS.type == "unix") {
    outside <- withr::local_tempfile()
    writeLines("NO CAMBIAR", outside)
    target <- file.path(root, "enlace.html")
    expect_true(file.symlink(outside, target))
    expect_error(racafeMailPreview("otp", .mail_data()$otp, target, cfg, overwrite = TRUE), "reemplazar")
    expect_identical(readLines(outside), "NO CAMBIAR")
  }
})


test_that("blastula compone HTML completo sin anidar documentos y usa creds_file", {
  cfg <- .mail_smtp_config(.mail_fixture())
  captured <- NULL
  credential_path <- NULL
  local_mocked_bindings(
    creds_file = function(file) { credential_path <<- file; "CREDENCIAL_SIMULADA" },
    smtp_send = function(email, from, to, subject, credentials, verbose) {
      captured <<- list(email = email, from = from, to = to, subject = subject, credentials = credentials,
                        verbose = verbose)
      invisible(TRUE)
    }, .package = "blastula")
  expect_true(racafeMailOTP("destino@example.org", "012345", "Ana", config = cfg))
  expect_s3_class(captured$email, "email_message")
  expect_identical(captured$to, "destino@example.org")
  expect_identical(captured$from, "origen@example.org")
  expect_identical(credential_path, cfg$credenciales)
  expect_identical(captured$credentials, "CREDENCIAL_SIMULADA")
  expect_false(captured$verbose)
  expect_match(captured$email$html_str, "012345", fixed = TRUE)
  expect_length(gregexpr("<!doctype html>", captured$email$html_str, fixed = TRUE)[[1]], 1L)
})

test_that("atajos conservan destino, código y nombre y delegan las cuatro plantillas", {
  cfg <- .mail_smtp_config(.mail_fixture())
  received <- list()
  local_mocked_bindings(.rmail_transport = function(content, para, config) {
    received[[length(received) + 1L]] <<- list(content = content, para = para)
    invisible(TRUE)
  })
  expect_true(racafeMailOTP("destino@example.org", "012345", "Ana", config = cfg))
  expect_true(racafeMailInvitacion("destino@example.org", "Ana", "https://example.org/?token=ficticio", "Hoy", cfg))
  expect_true(racafeMailConfirmacion("destino@example.org", "Ana", "Detalle", "Hoy", cfg))
  expect_true(racafeMailNotificacion("destino@example.org", "Ana", "Mensaje", cfg))
  expect_length(received, 4L)
  expect_true(all(vapply(received, function(x) x$para == "destino@example.org", logical(1))))
  expect_match(received[[1]]$content$html, "012345", fixed = TRUE)
  expect_match(received[[1]]$content$html, "Ana", fixed = TRUE)
  expect_match(received[[2]]$content$html, "token=ficticio", fixed = TRUE)
})

test_that("errores SMTP y salida del proveedor nunca revelan datos sensibles", {
  cfg <- .mail_smtp_config(.mail_fixture())
  local_mocked_bindings(.rmail_transport = function(...) {
    cat("OTP_SECRETO_012345 URL_SECRETA\n")
    message("PASSWORD_SECRETO")
    warning("CREDENCIAL_SECRETA")
    stop("TOKEN_SECRETO", call. = FALSE)
  })
  error <- NULL
  output <- capture.output(expect_message(expect_warning(
    error <- tryCatch(racafeMailOTP("destino@example.org", "012345", config = cfg), error = identity),
    NA), NA))
  expect_length(output, 0L)
  expect_s3_class(error, "racafe_mail_smtp_error")
  expect_identical(conditionMessage(error), "No se pudo completar el envio SMTP.")
  expect_null(conditionCall(error))
  expect_null(error$parent)
  expect_false(any(grepl("SECRETO|012345", unlist(error), perl = TRUE)))
})

test_that("validación previa impide envío con datos, destinatarios o credenciales inválidos", {
  cfg <- racafeMailConfig(.mail_fixture())
  local_mocked_bindings(.rmail_transport = function(...) stop("TRANSPORTE INESPERADO"))
  expect_error(racafeMailOTP("destino@example.org", "012345", config = cfg), "SMTP")
  cfg <- .mail_smtp_config(.mail_fixture())
  for (to in c("no-correo", "a@example.org\r\nBcc:b@example.org", "a@example.org,b@example.org")) {
    expect_error(racafeMailOTP(to, "012345", config = cfg), "correo")
  }
  expect_error(racafeMailOTP("destino@example.org", "INVALIDO", config = cfg), "digitos")
  expect_error(racafeMailInvitacion("destino@example.org", "Ana", "javascript:alert(1)", "Hoy", cfg), "HTTPS")
})


test_that("importar el adaptador stdin no envía ni consume la entrada", {
  script <- system.file("examples", "otp-stdin.R", package = "racafeMail")
  expect_true(nzchar(script))
  env <- new.env(parent = baseenv())
  local_mocked_bindings(racafeMailOTP = function(...) stop("NO ENVIAR"))
  expect_silent(sys.source(script, envir = env))
  expect_true(is.function(env$.rmail_stdin_main))
})


test_that("las cuatro plantillas internas del paquete comparten identidad configurable", {
  skip_if_not_installed("xml2")
  cfg <- racafeMailConfig(version = "0.2.0", color = "#123456", color_fondo = "#eeeeee",
                         color_texto = "#121212", empresa = "Marca & nueva", logo_url = "https://example.org/otro.png",
                         firma_url = "https://example.org/firma.png", pie = "Pie & nuevo", entorno = "true",
                         remitente = "", credenciales = "")
  local_mocked_bindings(.rmail_transport = function(...) stop("SIN SMTP"))
  for (name in names(.mail_data())) {
    mail <- racafeMailRender(name, .mail_data()[[name]], cfg)
    html <- xml2::read_html(mail$html)
    expect_length(xml2::xml_find_all(html, "//html"), 1L)
    expect_length(xml2::xml_find_all(html, "//script"), 0L)
    expect_length(xml2::xml_find_all(html, "//img[@src='https://example.org/otro.png']"), 1L)
    expect_length(xml2::xml_find_all(html, "//img[@src='https://example.org/firma.png']"), 1L)
    expect_match(xml2::xml_text(html), "Marca & nueva", fixed = TRUE)
    expect_match(xml2::xml_text(html), "Pie & nuevo", fixed = TRUE)
    expect_match(mail$html, "#123456", fixed = TRUE)
    expect_match(mail$html, "#eeeeee", fixed = TRUE)
    expect_match(mail$html, "#121212", fixed = TRUE)
    expect_false(any(vapply(c("#bf2838", "{{", "[["), grepl, logical(1), x = mail$html, fixed = TRUE)))
    expect_match(mail$html, racafeMailFirma(cfg), fixed = TRUE)
    file <- withr::local_tempfile(fileext = ".html")
    expect_true(file.exists(racafeMailPreview(name, .mail_data()[[name]], file, cfg)))
  }
})

test_that("el paquete funciona sin configuración, plantillas o repositorios externos", {
  withr::local_envvar(RACAFE_MAIL_HOME = "/no-existe/email", RACAFE_MAIL_TEST_HOME = "/no-existe/email",
                     RACAFE_MAIL_FROM = "", RACAFE_MAIL_CRED = "", RACAFE_MAIL_TEST_MODE = "false")
  local_mocked_bindings(.rmail_root = function(...) stop("NO LEER CARPETAS EXTERNAS"),
                       .rmail_read = function(...) stop("NO LEER HTML EXTERNO"),
                       .rmail_transport = function(...) stop("NO ENVIAR"))
  cfg <- racafeMailConfig()
  expect_null(cfg$ruta)
  expect_identical(cfg$version, as.character(utils::packageVersion("racafeMail")))
  expect_match(cfg$logo_url, "^data:image/png;base64,")
  expect_identical(cfg$logo_url, cfg$firma_url)
  for (name in names(.mail_data())) {
    mail <- racafeMailRender(name, .mail_data()[[name]], cfg)
    expect_false(grepl("raw.githubusercontent.com|Compartido", mail$html))
    expect_match(mail$html, "data:image/png;base64,", fixed = TRUE)
    expect_match(mail$html, racafeMailFirma(cfg), fixed = TRUE)
    file <- withr::local_tempfile(fileext = ".html")
    expect_true(file.exists(racafeMailPreview(name, .mail_data()[[name]], file, cfg)))
  }
  expect_error(racafeMailConfig(version = "0.1.0"), "version")
})

test_that("blastula convierte logo y firma incluidos en una única imagen CID", {
  cfg <- racafeMailConfig()
  raw <- base64enc::base64decode(sub("^data:image/png;base64,", "", cfg$logo_url))
  file <- system.file("img", "logo.png", package = "racafeMail")
  expect_identical(raw, readBin(file, "raw", n = file.info(file)$size))
  for (name in names(.mail_data())) {
    message <- .rmail_compose(racafeMailRender(name, .mail_data()[[name]], cfg)$html)
    expect_s3_class(message, "email_message")
    expect_length(message$images, 1L)
    expect_match(message$html_str, "cid:", fixed = TRUE)
    expect_false(grepl('src="data:', message$html_str, fixed = TRUE))
    expect_identical(unname(message$images[[1]]),
                     structure(sub("^data:image/png;base64,", "", cfg$logo_url), content_type = "image/png"))
  }
})

test_that("personalización en R y PNG local conserva colores, firma y validación segura", {
  file <- system.file("img", "logo.png", package = "racafeMail")
  cfg <- racafeMailConfig(logo_url = file, empresa = "Marca nueva", color = "#123456", pie = "Pie nuevo")
  expect_match(cfg$logo_url, "^data:image/png;base64,")
  expect_identical(cfg$logo_url, cfg$firma_url)
  html <- racafeMailRender("notificacion", .mail_data()$notificacion, cfg)$html
  expect_match(html, "Marca nueva", fixed = TRUE)
  expect_match(html, "#123456", fixed = TRUE)
  expect_match(html, "Pie nuevo", fixed = TRUE)
  for (value in c("data:image/svg+xml;base64,PHN2Zz4=", "data:image/png;base64,%%%%",
                  "data:image/png;base64,QUJD")) expect_error(racafeMailConfig(logo_url = value))
  expect_error(racafeMailPreview("otp", .mail_data()$otp, file, cfg, overwrite = TRUE), "reemplazar")
})
