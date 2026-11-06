test_that("Los alias CajaValor delegan sin cambiar la API anterior", {
  skip_if_not_installed("racafeModulos", "0.3.0")
  expect_identical(as.character(cajaValor_ui("ventas", texto = "Café")),
    as.character(racafeModulos::cajaValor_ui("ventas", texto = "Café")))
  valor <- shiny::reactiveVal(1234.56)
  shiny::testServer(cajaValor_server, args = list(valor_r = valor, formato = "numero"), {
    session$flushReact()
    expect_match(output$valor_display$html, "1,234.56", fixed = TRUE)
    session$setInputs(detalle = 2)
    expect_equal(shiny::isolate(session$returned$click_detalle()), 2)
    valor(0)
    session$flushReact()
    expect_match(output$valor_display$html, "0.00", fixed = TRUE)
  })
})
