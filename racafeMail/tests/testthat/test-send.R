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
