# Galería y extensiones compatibles ----
test_that("las variantes explícitas tienen precedencia y conservan los defaults", {
  expect_match(as.character(racafeForm()), "rf-form--bordered", fixed = TRUE)
  expect_false(grepl("rf-form--bordered", as.character(racafeForm(unstyled = TRUE)), fixed = TRUE))
  for (variant in c("plain", "soft", "bordered")) {
    expect_match(as.character(racafeForm(bordered = FALSE, unstyled = TRUE, variant = variant)),
                 paste0("rf-surface--", variant), fixed = TRUE)
    expect_match(as.character(racafeFormGroup(variant = variant)), paste0("rf-surface--", variant), fixed = TRUE)
  }
  expect_error(racafeForm(variant = "otro"))
  expect_error(racafeFormGroup(variant = "otro"))
  expect_match(as.character(racafeFormMessage("Aviso", size = "comfortable", icon = "circle-info")),
               "rf-message--comfortable", fixed = TRUE)
})
test_that("los catálogos conservan grupos y valores sin duplicar el vacío", {
  choices <- list(Grupo = c("Ninguno" = "", "Café" = "cafe"), Otro = c("Vacío" = "", "Repetido" = "cafe"))
  result <- racafeForms:::.rf_select_choices(choices, TRUE)
  expect_identical(unname(unlist(result)), c("", "cafe"))
  expect_identical(names(result), c("Grupo", "Otro"))
  expect_identical(unname(unlist(racafeForms:::.rf_select_choices(choices, FALSE))), c("", "cafe", "cafe"))
  html <- as.character(racafeInputSelect("x", choices = c("A" = "a", "A2" = "a"), unique_choices = TRUE))
  expect_equal(lengths(regmatches(html, gregexpr('value="a"', html, fixed = TRUE))), 1)
})
test_that("habilitación central envía el namespace una sola vez", {
  shiny::testServer(function(input, output, session) {
    messages <- list()
    session$sendCustomMessage <- function(type, message) messages[[length(messages) + 1L]] <<- list(type, message)
    racafeUpdateEnabled(session, "campo", FALSE)
    racafeEnable("campo", session)
    racafeDisable("campo", session)
    racafeToggle("campo", TRUE, session)
    expect_identical(messages[[1]][[1]], "racafe-enabled")
    expect_identical(messages[[1]][[2]]$id, session$ns("campo"))
    expect_identical(vapply(messages, function(x) x[[2]]$enabled, logical(1)), c(FALSE, TRUE, FALSE, TRUE))
    expect_error(racafeUpdateEnabled(session, "campo", NA))
    racafeUpdateEnabled(session, session$ns("campo"), TRUE)
    expect_identical(messages[[5]][[2]]$id, session$ns("campo"))
  }, {})
})
test_that("el inventario cubre exportaciones y los fragmentos solo contienen UI y server", {
  catalog <- racafeForms:::.rf_gallery_catalog()
  expect_setequal(catalog$name, c(getNamespaceExports("racafeForms"), "Formulario completo"))
  expect_false(anyDuplicated(catalog$name) > 0)
  config <- racafeForms:::.rf_gallery_defaults()
  assets <- racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"), global_css = FALSE)
  for (name in catalog$name) {
    code <- racafeForms:::.rf_gallery_code(name, config)
    expect_silent(parse(text = code))
    expect_true(all(nchar(strsplit(code, "\n", fixed = TRUE)[[1]]) <= 120), info = name)
    expect_match(code, "# UI ----", fixed = TRUE, info = name)
    expect_match(code, "# Server ----", fixed = TRUE, info = name)
    expect_false(grepl("shinyApp|moduleServer|system.file|assets <-|example_ui", code), info = name)
    example <- racafeForms:::.rf_gallery_example(name, config)
    if (!racafeForms:::.rf_gallery_details(name)$resource) {
      expect_silent(htmltools::renderTags(eval(parse(text = example$ui))))
    }
    ui <- racafeForms:::.rf_gallery_ui(name, config, shiny::NS("parent-child"), assets)
    expect_true(inherits(ui, "shiny.tag.list"), info = name)
  }
  skip_if_not_installed("bs4Dash")
  expect_s3_class(racafeFormsGallery(), "shiny.appobj")
})

test_that("los auxiliares explican su función y los fragmentos de servidor son utilizables", {
  helpers <- c("racafeValidateForm", "racafeShowValidation", "racafeResetValidation", "racafeUpdateNumeric",
               "racafeFormsDependencies", "racafeSharedAssets", "racafeFormsGallery")
  for (name in helpers) {
    details <- racafeForms:::.rf_gallery_details(name)
    expect_true(nzchar(details$description), info = name)
    expect_false(identical(details$kind, "Componente de interfaz"), info = name)
  }
  config <- racafeForms:::.rf_gallery_defaults()
  expect_identical(racafeForms:::.rf_gallery_example("racafeInputText", config)$ui,
                   'racafeForms::racafeInputText("value", "Nombre")')
  for (name in c("racafeValidateForm", "racafeShowValidation", "racafeFormValidation", "racafeResetValidation")) {
    code <- racafeForms:::.rf_gallery_example(name, config)$server
    shiny::testServer(function(input, output, session) eval(parse(text = code), envir = environment()), {
      session$setInputs(value = "", save = 0)
      session$setInputs(save = 1)
      if (name == "racafeValidateForm") expect_match(output$result, "FALSE", fixed = TRUE)
      session$setInputs(value = "Cliente demo", save = 2)
      if (name == "racafeValidateForm") expect_match(output$result, "TRUE", fixed = TRUE)
      if (name == "racafeResetValidation") session$setInputs(reset = 1)
    })
  }
})

test_that("las variaciones se reflejan en la vista previa y en el fragmento de UI", {
  config <- racafeForms:::.rf_gallery_defaults()
  config$button_variant <- "warning"
  config$button_size <- "lg"
  config$badge_variant <- "danger"
  config$badge_size <- "md"
  config$surface <- "highlighted"
  config$date_view <- "month"
  config$rows <- 8L
  config$inline <- FALSE
  config$checked <- TRUE
  expected <- c(racafeButton = "rf-btn--warning", racafeStatusBadge = "rf-status-badge--danger",
                racafeCard = "rf-card--highlighted", racafeInputDate = "month", racafeInputTextArea = 'rows="8"',
                racafeInputSwitch = 'checked="checked"', racafeInputCheckbox = 'checked="checked"')
  assets <- racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"), global_css = FALSE)
  for (name in names(expected)) {
    preview <- htmltools::renderTags(racafeForms:::.rf_gallery_ui(name, config, shiny::NS("demo"), assets))$html
    snippet <- racafeForms:::.rf_gallery_example(name, config)$ui
    html <- htmltools::renderTags(eval(parse(text = snippet)))$html
    expect_match(preview, expected[[name]], fixed = TRUE, info = name)
    expect_match(html, expected[[name]], fixed = TRUE, info = name)
  }
  expect_match(racafeForms:::.rf_gallery_example("racafeInputRadio", config)$ui, "inline = FALSE", fixed = TRUE)
})

test_that("la instantánea carga solo componentes sin reglas generales", {
  assets <- racafeSharedAssets(system.file("gallery-assets", package = "racafeForms"), global_css = FALSE)
  deps <- htmltools::renderTags(assets)$dependencies
  shared <- Filter(function(dep) dep$name == "racafeSharedAssets", deps)[[1]]
  expect_match(shared$stylesheet, "/components.css", fixed = TRUE)
  expect_false(file.exists(system.file("gallery-assets", "Styles", "style.css", package = "racafeForms")))
  tokens <- readLines(system.file("gallery-assets", "Styles", "tokens.css", package = "racafeForms"))
  expect_true(any(grepl("#DA291C", tokens, fixed = TRUE)))
})
