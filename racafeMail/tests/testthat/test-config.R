test_that("configuración R usa archivo, overrides, entorno y versión sin credenciales", {
  root <- .mail_fixture()
  cfg <- racafeMailConfig(root, version = "0.1.0")
  expect_s3_class(cfg, "racafe_mail_config")
  expect_identical(cfg$color, "#bf2838")
  expect_identical(cfg$version, "0.1.0")
  expect_false(cfg$pruebas)
  withr::local_envvar(RACAFE_MAIL_HOME = root)
  expect_identical(racafeMailConfig()$ruta, normalizePath(root))
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
