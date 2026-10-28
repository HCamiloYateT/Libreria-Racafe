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
