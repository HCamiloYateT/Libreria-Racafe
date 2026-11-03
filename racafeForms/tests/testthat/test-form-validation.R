test_that("required en la UI es un esquema de validación de servidor", {
  ui <- racafeForm(id = "form", ns = shiny::NS("uno"),
    racafeInputText("nombre", shiny::tags$strong("Nombre"), required = TRUE, ns = shiny::NS("uno")),
    racafeInputNumeric("importe", "Importe", min = 1, max = 100, ns = shiny::NS("uno")),
    racafeInputSwitch("activo", "Activo", ns = shiny::NS("uno")))
  first <- racafeValidateForm(list(nombre = " ", importe = 101, activo = FALSE), ui, ns = shiny::NS("uno"))
  expect_false(first$valid)
  expect_equal(first$pending, c("nombre", "importe"))
  expect_equal(first$ids[["nombre"]], "uno-nombre")
  expect_equal(first$form_id, "uno-form")
  expect_equal(first$labels[["nombre"]], "Nombre")
  expect_false(first$values$activo)
  expect_true(racafeValidateForm(list(nombre = "Correcto", importe = 1, activo = TRUE), ui,
                               ns = shiny::NS("uno"))$valid)
})

test_that("se validan fechas, rangos y valores realmente numéricos", {
  fields <- list(fecha = list(kind = "date", required = TRUE),
                 rango = list(kind = "range"), importe = list(kind = "numeric", min = 0),
                 fecha_hora = list(kind = "datetime"))
  bad <- racafeValidateForm(list(fecha = "no-fecha", rango = as.Date(c("2026-02-02", "2026-01-01")),
    importe = Inf, fecha_hora = "no-fecha"), fields)
  expect_equal(bad$pending, names(fields))
  good <- list(fecha = Sys.Date(), rango = as.Date(c("2026-01-01", "2026-02-02")), importe = 0, fecha_hora = Sys.time())
  expect_true(racafeValidateForm(good, fields)$valid)
  good$fecha <- as.Date(NA)
  expect_false(racafeValidateForm(good, fields)$valid)
  good$fecha <- Sys.Date(); good$importe <- "1,234.56"
  expect_false(racafeValidateForm(good, fields)$valid)
})

test_that("reglas condicionales comparten valores sin cambiar tipos", {
  fields <- list(contactar = list(kind = "logical"), correo = list(), aceptar = list(kind = "logical", required = TRUE))
  required <- list(correo = function(values) isTRUE(values$contactar))
  rules <- list(aceptar = function(value, values) if (isTRUE(value)) TRUE else "Debe aceptar.")
  result <- racafeValidateForm(list(contactar = FALSE, correo = "", aceptar = FALSE), fields,
                               rules = rules, required = required)
  expect_equal(result$pending, "aceptar")
  result <- racafeValidateForm(list(contactar = TRUE, correo = "", aceptar = TRUE), fields, required = required)
  expect_equal(result$pending, "correo")
  expect_true(racafeValidateForm(list(contactar = FALSE, correo = "", aceptar = TRUE), fields,
                               rules = rules, required = required)$valid)
  expect_error(racafeValidateForm(list(), fields, required = list(otro = TRUE)), "nombrar campos")
  expect_error(racafeValidateForm(list(), fields, required = list(correo = NA)))
})

test_that("la vinculación difiere errores y limpia al corregir", {
  shiny::testServer(function(input, output, session) {
    validation <- racafeFormValidation(input,
      racafeForm(id = "form", racafeInputText("nombre", "Nombre", required = TRUE)), session = session)
  }, {
    session$flushReact()
    expect_false(validation$attempted())
    expect_null(validation$result())
    expect_false(validation$validate())
    expect_true(validation$attempted())
    session$setInputs(nombre = "Correcto")
    expect_true(validation$result()$valid)
    expect_true(validation$validate())
  })
})

test_that("los esquemas dinámicos no retienen campos removidos", {
  active <- TRUE
  form <- function() racafeForm(id = "f", racafeInputText("base", "Base"),
    if (active) racafeInputText("extra", "Extra", required = TRUE))
  expect_equal(racafeValidateForm(list(base = ""), form)$pending, "extra")
  active <- FALSE
  expect_true(racafeValidateForm(list(base = ""), form)$valid)
})

test_that("fecha-hora conserva min/max y zona; los títulos HTML son compactables", {
  value <- as.POSIXct("2026-11-03 10:30:00", tz = "America/Bogota")
  tag <- racafeInputDateTime("fecha", "Fecha y hora", value, min = value - 3600, max = value + 3600)
  html <- as.character(tag)
  expect_match(html, '"timepicker":true', fixed = TRUE)
  expect_match(html, '"tz":"America/Bogota"', fixed = TRUE)
  expect_match(html, 'data-rf-kind="datetime"', fixed = TRUE)
  result <- racafeValidateForm(list(fecha = value), racafeForm(tag))
  expect_true(result$valid)
  expect_false(racafeValidateForm(list(fecha = value - 7200), racafeForm(tag))$valid)
  expect_error(racafeInputDateTime("f", "Fecha", tz = "no-existe"))
  panel <- as.character(racafePanel(shiny::tags$h1("Título", shiny::tags$em("con contenido"))))
  expect_match(panel, "con contenido", fixed = TRUE)
  expect_false(grepl("<h1", panel, fixed = TRUE))
})

test_that("los tipos enviados por el cliente se comprueban en el servidor", {
  fields <- list(contactar = list(kind = "logical"), texto = list(), selector = list(kind = "select"))
  expect_false(racafeValidateForm(list(contactar = "true", texto = "a", selector = "A"), fields)$valid)
  expect_false(racafeValidateForm(list(contactar = TRUE, texto = c("a", "b"), selector = "A"), fields)$valid)
  expect_true(racafeValidateForm(list(contactar = FALSE, texto = "a", selector = c("A", "B")), fields)$valid)
})

test_that("el contenido de descarga se revalida y recibe datos numéricos", {
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  allowed <- FALSE; captured <- NULL; calls <- 0L
  unlockBinding("registerDownload", session)
  session$registerDownload <- function(name, filename, contentType, content) {
    captured <<- content
    "download-test"
  }
  validation <- list(validate = function() allowed, check = function() {
    list(valid = allowed, values = list(importe = 1234.56))
  })
  racafeDownloadHandler("archivo", validation, "prueba.txt", function(file, values) {
    expect_type(values$importe, "double")
    calls <<- calls + 1L
    writeLines(as.character(values$importe), file)
  }, session = session)
  session$flushReact()
  expect_true(is.function(captured))
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  expect_error(captured(file), class = "shiny.silent.error")
  expect_equal(calls, 0L)
  allowed <- TRUE
  captured(file)
  expect_equal(calls, 1L)
  expect_equal(readLines(file), "1234.56")
})

test_that("mostrar errores solo envía campos y mensajes, sin valores del formulario", {
  messages <- list()
  session <- list(sendCustomMessage = function(type, message) messages[[type]] <<- message)
  result <- racafeValidateForm(list(nombre = ""), list(nombre = list(required = TRUE)))
  racafeShowValidation(session, result)
  expect_equal(messages[["racafe-validation"]]$errors[[1]]$id, "nombre")
  expect_false("values" %in% names(messages[["racafe-validation"]]))
  expect_error(racafeShowValidation(session, list(valid = TRUE)))
})


test_that("los espacios Unicode no satisfacen obligatoriedad", {
  expect_false(racafeValidateForm(list(nombre = "\u00a0"), list(nombre = list(required = TRUE)))$valid)
})


test_that("las reglas dinámicas siguen el esquema visible sin claves antiguas", {
  extra <- TRUE
  form <- function() racafeForm(racafeInputText("base", "Base"),
    if (extra) racafeInputText("extra", "Extra"))
  rules <- function() if (extra) list(extra = function(value, values) "Revisar referencia") else list()
  expect_false(racafeValidateForm(list(base = "a", extra = "b"), form, rules = rules)$valid)
  extra <- FALSE
  expect_true(racafeValidateForm(list(base = "a"), form, rules = rules)$valid)
})


test_that("los esquemas rechazan configuraciones ambiguas y aceptan condiciones", {
  expect_error(racafeValidateForm(list(x = "1"), list(x = list(kind = "numerico"))), "kind")
  expect_error(racafeValidateForm(list(x = ""), list(x = list(required = 1))), "required")
  schema <- list(x = list(required = function(values) TRUE))
  expect_false(racafeValidateForm(list(x = ""), schema)$valid)
})
