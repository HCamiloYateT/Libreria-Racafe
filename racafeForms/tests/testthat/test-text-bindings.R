test_that("los cuatro tipos mantienen el elemento que reconoce el binding nativo", {
  for (type in c("text", "email", "tel", "password")) {
    tag <- racafeInputText("campo", "Campo", "Inicial", type = type, ns = shiny::NS("mod"),
                           required = TRUE, readonly = TRUE, disabled = TRUE, help = "Ayuda", error = "Error")
    input <- htmltools::tagQuery(tag)$find("input")$selectedTags()[[1]]
    expect_identical(input$attribs$id, "mod-campo")
    expect_identical(input$attribs$type, if (type == "password") "password" else "text")
    expect_identical(input$attribs$value, "Inicial")
    for (attribute in c("required", "readonly", "disabled")) expect_identical(input$attribs[[attribute]], attribute)
    expect_identical(input$attribs[["aria-invalid"]], "true")
    expect_identical(input$attribs[["aria-describedby"]], "mod-campo_help mod-campo_error")
    if (type %in% c("email", "tel")) {
      expect_identical(input$attribs$inputmode, type)
      expect_identical(input$attribs$autocomplete, type)
    }
  }
})

test_that("autocompletado explícito y catálogo conservan la API y namespace", {
  for (type in c("email", "tel")) {
    tag <- racafeInputText("dato", "Dato", type = type, ns = shiny::NS("m"),
                           autocomplete = "off", choices = c("Uno", "Dos"))
    input <- htmltools::tagQuery(tag)$find("input")$selectedTags()[[1]]
    expect_identical(input$attribs$autocomplete, "off")
    expect_identical(input$attribs$list, "m-dato_catalog")
    expect_identical(input$attribs$inputmode, type)
  }
})

test_that("el módulo ejecutable lee los IDs locales y conserva actualizadores nativos", {
  env <- new.env(parent = globalenv())
  sys.source(system.file("examples", "text-input-bindings.R", package = "racafeForms"), envir = env)
  shiny::testServer(env$moduleServer, args = list(id = "prueba"), {
    session$setInputs(text = "Texto", email = "a@example.org", tel = "+57 3001234567", password = "ficticia")
    expect_identical(valores()$text, "Texto")
    expect_identical(valores()$email, "a@example.org")
    expect_identical(valores()$tel, "+57 3001234567")
    expect_identical(valores()$password, "ficticia")
    session$setInputs(email = "b@example.org", tel = "+57 3019876543")
    expect_identical(valores()$email, "b@example.org")
    expect_identical(valores()$tel, "+57 3019876543")
  })
})
