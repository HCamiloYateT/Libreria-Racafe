test_that("los resúmenes escapan datos, conservan valores y omiten campos opcionales", {
  table <- as.character(racafeSummaryTable(data.frame(Producto = "<script>alert(1)</script>", Importe = 1234.56)))
  expect_match(table, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>alert(1)</script>", table, fixed = TRUE))
  expect_match(table, "1,234.56", fixed = TRUE)
  expect_match(as.character(racafeSummaryTable(data.frame())), "Sin registros", fixed = TRUE)
  expect_error(racafeSummaryTable(data.frame(x = 1), digits = 1.5))
  expect_error(racafeSummaryTable(list(x = 1)))
  expect_error(racafeSummaryTable(data.frame(x = I(list(1)))))
  expect_match(as.character(racafeMoneyTotal("Total", -1234.5)), "COP -1,234.50", fixed = TRUE)
  expect_error(racafeMoneyTotal("Total", Inf))
  expect_error(racafeMoneyTotal("Total", 1, digits = -1))
  modal <- as.character(racafeConfirmation("save", list(Cliente = "Demo", Omitido = NULL),
    racafeNoticeList(c("Revisar", "<b>Texto</b>")), ns = shiny::NS("parent-child"), cancel_id = "cancel"))
  expect_match(modal, "rf-info-grid", fixed = TRUE)
  expect_match(modal, 'id="parent-child-save"', fixed = TRUE)
  expect_match(modal, 'id="parent-child-cancel"', fixed = TRUE)
  expect_match(modal, 'data-dismiss="modal"', fixed = TRUE)
  expect_false(grepl("Omitido", modal, fixed = TRUE))
  expect_match(modal, "&lt;b&gt;Texto&lt;/b&gt;", fixed = TRUE)
  expect_error(racafeConfirmation("x", list("sin nombre")))
  expect_match(as.character(racafeStack(racafeCard("Uno"), racafeCard("Dos"), spacing = "compact")),
               "rf-stack--compact", fixed = TRUE)
})

test_that("las advertencias no bloquean ni duplican errores y los opcionales ausentes son válidos", {
  schema <- list(name = list(required = TRUE), optional = list(), number = list(kind = "numeric"))
  warnings <- list(name = list(function(value, values) "Aviso 1", function(value, values) "Aviso 2"))
  result <- racafeValidateForm(list(name = "Ana"), schema, warnings = warnings)
  expect_true(result$valid)
  expect_length(result$errors, 0)
  expect_identical(unname(result$warnings), "Aviso 1")
  expect_null(result$values$optional)
  expect_null(result$values$number)
  bad <- racafeValidateForm(list(name = ""), schema, warnings = warnings)
  expect_false(bad$valid)
  expect_length(bad$warnings, 0)
  expect_length(bad$errors, 1)
  expect_error(racafeValidateForm(list(name = "A"), schema, warnings = list(name = "incorrecta")))
})

test_that("reset reinicia el intento y los mensajes sin duplicar el controlador ni perder valores", {
  shiny::testServer(function(input, output, session) {
    messages <- list()
    session$sendCustomMessage <- function(type, message) messages[[length(messages) + 1L]] <<- message
    schema <- shiny::reactive({
      fields <- list(name = list(required = TRUE), optional = list())
      if (isTRUE(input$dynamic)) fields$extra <- list(required = TRUE)
      fields
    })
    validation <- racafeFormValidation(input, schema, session,
      warnings = list(name = function(value, values) "Aviso"))
    session$setInputs(name = "", dynamic = FALSE)
    expect_false(validation$attempted())
    expect_false(validation$validate())
    session$flushReact()
    expect_true(validation$attempted())
    expect_true(tail(messages, 1)[[1]]$inline)
    session$setInputs(name = "Ana")
    expect_true(validation$check()$valid)
    expect_length(validation$result()$warnings, 1)
    racafeResetValidation(validation)
    session$flushReact()
    expect_false(validation$attempted())
    expect_null(validation$result())
    expect_false(tail(messages, 1)[[1]]$inline)
    expect_identical(input$name, "Ana")
    expect_identical(validation, racafeFormValidation(input, schema, session))
    session$setInputs(dynamic = TRUE)
    expect_false(validation$attempted())
    expect_false(validation$validate())
    expect_identical(validation$check()$pending, "extra")
    session$setInputs(dynamic = FALSE)
    expect_true(validation$check()$valid)
  }, {})
  expect_error(racafeResetValidation(list()))
})

test_that("la copia local de controles coincide con la fuente de la galería", {
  read <- function(...) readBin(system.file(..., package = "racafeForms"), "raw", n = 1000000)
  expect_identical(read("www", "racafe-controls.css"), read("gallery-assets", "Styles", "racafe-controls.css"))
})

test_that("el esquema conserva el formulario raíz al contener módulos anidados", {
  ns <- shiny::NS("parent")
  ui <- racafeForm(id = "form", ns = ns, racafeInputText("name", "Nombre", ns = ns),
    racafeForm(id = "form", ns = shiny::NS(ns("nested")),
      racafeInputText("child", "Hijo", ns = shiny::NS(ns("nested")))))
  result <- racafeValidateForm(list(name = "Padre", `nested-child` = "Hijo"), ui, ns = ns)
  expect_identical(result$form_id, "parent-form")
  expect_setequal(names(result$values), c("name", "nested-child"))
  expect_true(result$valid)
})
