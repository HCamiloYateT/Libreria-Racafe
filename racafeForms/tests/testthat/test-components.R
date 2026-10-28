# Construcción de componentes ----
test_that("paneles y tarjetas no imponen altura, iconos ni clics", {
  panel <- as.character(racafePanel("Título", "Contenido"))
  expect_match(panel, "rf-panel", fixed = TRUE)
  expect_false(grepl("min-height", panel, fixed = TRUE))
  expect_false(grepl("<i", panel, fixed = TRUE))
  expect_match(as.character(racafePanel(min_height = 100)), "min-height:100px", fixed = TRUE)
  expect_false(grepl('data-rf-action', as.character(racafeCard("Título", "Dato")), fixed = TRUE))
  clickable <- as.character(racafeCard("Acción", clickable = TRUE, id = "abrir", ns = shiny::NS("m")))
  expect_match(clickable, 'data-rf-action="m-abrir"', fixed = TRUE)
  expect_match(clickable, 'tabindex="0"', fixed = TRUE)
  expect_error(racafeCard(clickable = TRUE))
  expect_error(racafePanel(variant = "otro"), "arg")
  expect_error(racafeCard(clickable = NA), "lógico")
  for (columns in 1:4) {
    expect_match(as.character(racafeMetricGrid(racafeMetric(1, "Uno"), columns = columns)),
                 paste0("rf-grid--", columns), fixed = TRUE)
    expect_match(as.character(racafeInfoGrid(racafeInfoItem("Dato", NULL), columns = columns)),
                 "Sin información", fixed = TRUE)
  }
  expect_error(racafeInfoGrid(columns = 0), "columns")
  expect_error(racafeMetricGrid(columns = 2.5), "columns")
  expect_match(as.character(racafeInfoItem("Dato", "<script>")), "&lt;script&gt;", fixed = TRUE)
})

test_that("modales usan las clases de Compartido, sin duplicar anchos", {
  for (size in c("ventana", paste0("subventana", 1:5), "aviso")) {
    html <- as.character(racafeModal("Contenido", size = size))
    expect_match(html, paste("rf-modal", size), fixed = TRUE)
    expect_false(grepl("max-width", html, fixed = TRUE))
    expect_false(grepl("modal-header", html, fixed = TRUE))
  }
  html <- as.character(racafeModal(title = "Título", footer = NULL))
  expect_match(html, "modal-header", fixed = TRUE)
  expect_false(grepl("modal-footer", html, fixed = TRUE))
  expect_error(racafeModal(size = "otro"), "arg")
  expect_error(racafeModal(easy_close = NA), "lógico")
  expect_match(as.character(racafeModalFooter()), 'data-dismiss="modal"', fixed = TRUE)
})

test_that("acciones y descargas mantienen el enlace nativo e IDs únicos", {
  tag <- racafeActionBar(left = racafeButton("guardar", "Guardar", ns = shiny::NS("m")),
                         right = racafeButtonDownload("archivo", ns = shiny::NS("m")))
  html <- as.character(tag)
  expect_match(html, 'id="m-guardar"', fixed = TRUE)
  expect_match(html, 'id="m-archivo"', fixed = TRUE)
  expect_match(html, "shiny-download-link", fixed = TRUE)
  expect_false(grepl("onclick", html, fixed = TRUE))
  expect_equal(sum(gregexpr('fa-download', html, fixed = TRUE)[[1]] > 0), 1)
  expect_error(racafeActionBar(wrap = NA), "lógico")
  old <- racafeFormFooter(racafeButton("a", "A"), help = "Ayuda")
  expect_match(as.character(old), "rf-footer-actions", fixed = TRUE)
  expect_match(as.character(racafeFormFooter(left = "L", right = "R")), "rf-action-bar", fixed = TRUE)
})

test_that("campos conservan argumentos posicionales y estados accesibles", {
  text <- as.character(racafeInputText("x", "Nombre", "Valor", "Ayuda", "text", TRUE, "Detalle", "100%",
                                      shiny::NS("m")))
  expect_match(text, 'id="m-x"', fixed = TRUE)
  expect_match(text, 'aria-required="true"', fixed = TRUE)
  expect_match(text, 'aria-describedby="m-x_help"', fixed = TRUE)
  html <- as.character(racafeInputText("x", "Nombre", ns = shiny::NS("m"), readonly = TRUE,
                                      error = "Error", choices = c("Uno", "Dos"), autocomplete = "organization"))
  for (value in c('readonly="readonly"', 'aria-invalid="true"', 'id="m-x_catalog"', 'list="m-x_catalog"')) {
    expect_match(html, value, fixed = TRUE)
  }
  expect_error(racafeInputText("x", "Nombre", disabled = NA), "lógico")
  for (tag in list(racafeInputNumeric("n", "Número", disabled = TRUE),
                   racafeInputDate("f", disabled = TRUE), racafeInputDateRange("r", disabled = TRUE),
                   racafeInputSelect("s", choices = "a", disabled = TRUE))) {
    expect_match(as.character(tag), 'disabled="disabled"', fixed = TRUE)
  }
  html <- as.character(racafeInputReadOnly("ref", "Referencia", "", copyable = TRUE, ns = shiny::NS("m")))
  expect_match(html, 'id="m-ref"', fixed = TRUE)
  expect_match(html, 'data-rf-copy-target="m-ref"', fixed = TRUE)
  expect_match(html, 'id="m-ref_copy"', fixed = TRUE)
  expect_false(grepl('disabled=', html, fixed = TRUE))
  expect_match(as.character(racafeInputReadOnly("ref", "Referencia", multiline = TRUE)), "<textarea", fixed = TRUE)
  expect_match(as.character(racafeForm(id = "f", ns = shiny::NS("m"), unstyled = TRUE)), 'id="m-f"', fixed = TRUE)
})

test_that("mensajes son accesibles y los detalles técnicos son optativos", {
  for (type in c("info", "success", "warning", "error", "muted", "loading", "empty")) {
    expect_match(as.character(racafeFormMessage("Mensaje", type)), paste0("rf-message--", type), fixed = TRUE)
  }
  expect_match(as.character(racafeFormMessage("Cargando", "loading")), 'aria-busy="true"', fixed = TRUE)
  expect_match(as.character(racafeFormMessage("Error", "error", details = "<script>")),
               "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("Detalles técnicos", as.character(racafeFormMessage("Error", "error")), fixed = TRUE))
})

# Mensajes de servidor sin doble namespace ----
test_that("el portapapeles y reset usan el namespace de la sesión y no envían éxito anticipado", {
  message <- NULL
  session <- list(ns = shiny::NS("m"), sendCustomMessage = function(type, value) message <<- list(type, value))
  racafeCopyText(session, "copiar", "Texto")
  expect_identical(message[[1]], "rf-copy-text")
  expect_identical(message[[2]]$result_id, "m-copiar_copy_result")
  expect_null(message[[2]]$success)
  racafeCopyImage(session, "imagen", "img/logo.png")
  expect_identical(message[[1]], "rf-copy-image")
  expect_identical(message[[2]]$result_id, "m-imagen_copy_result")
  racafeResetForm(session, "form", clear = TRUE)
  expect_identical(message[[2]]$id, "m-form")
  expect_true(message[[2]]$clear)
  expect_error(racafeCopyText(session, "x", NA), "cadena")
  expect_error(racafeCopyButton("x"), "exactamente")
  expect_error(racafeCopyButton("x", text = "a", target = "b"), "exactamente")
  expect_match(as.character(racafeCopyButton("x", text = "")), 'data-rf-copy-text=""', fixed = TRUE)
})

test_that("las firmas de 0.2.1 conservan sus argumentos y orden", {
  baseline <- new.env(parent = baseenv())
  sys.source(testthat::test_path("..", "fixtures", "public-signatures-0.2.1.R"), envir = baseline)
  for (name in names(baseline$.baseline_signatures)) {
    expected <- baseline$.baseline_signatures[[name]]
    actual <- names(formals(getExportedValue("racafeForms", name)))
    expect_identical(head(actual, length(expected)), expected, info = name)
  }
  expect_match(as.character(racafeFormMessage("Error", type = "e")), "rf-message--error", fixed = TRUE)
})

test_that("las alertas nuevas insertan etiquetas reales y no cadenas HTML escapadas", {
  session <- shiny::MockShinySession$new()
  on.exit(session$close())
  messages <- list()
  session$sendInsertUI <- function(...) messages[[length(messages) + 1L]] <<- list(...)
  for (type in c("loading", "empty")) {
    messages <- list()
    racafeAlert(session, "Estado", "Mensaje", type, details = "<detalle>")
    html <- paste(vapply(messages, function(x) x$content$html, character(1)), collapse = "")
    expect_match(html, paste0("rf-message--", type), fixed = TRUE)
    expect_match(html, "&lt;detalle&gt;", fixed = TRUE)
    expect_false(grepl("&lt;details", html, fixed = TRUE))
  }
})


test_that("readonly admite valores aún no cargados y referencias escalares", {
  for (value in list(NULL, NA_character_, character())) {
    expect_match(as.character(racafeInputReadOnly("r", "Referencia", value)), 'value=""', fixed = TRUE)
  }
  expect_match(as.character(racafeInputReadOnly("r", "Referencia", 123)), 'value="123"', fixed = TRUE)
  expect_error(racafeInputReadOnly("r", "Referencia", c("a", "b")), "cadena")
})
