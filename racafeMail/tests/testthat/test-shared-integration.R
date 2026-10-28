test_that("las cuatro plantillas reales de Compartido comparten identidad configurable", {
  root <- Sys.getenv("RACAFE_MAIL_TEST_HOME")
  skip_if(!nzchar(root), "Defina RACAFE_MAIL_TEST_HOME con una copia versionada de Compartido/email.")
  skip_if_not_installed("xml2")
  cfg <- racafeMailConfig(root, version = "0.1.0", color = "#123456", color_fondo = "#eeeeee",
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
