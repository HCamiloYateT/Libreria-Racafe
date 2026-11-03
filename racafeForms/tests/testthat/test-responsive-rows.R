test_that("las filas conservan el HTML heredado cuando responsive se omite", {
  field <- shiny::tags$input(id = "x")
  for (columns in 1:4) {
    expected <- shiny::tags$div(class = paste("rf-row", paste0("rf-row--", columns), NULL),
      list(shiny::tags$div(class = "rf-col", field)))
    expect_identical(as.character(racafeFormRow(field, columns = columns)), as.character(expected))
    expect_identical(as.character(racafeFormRow(field, columns = columns, responsive = NULL)),
                     as.character(expected))
  }
})

test_that("las modalidades explícitas generan contenedores propios sin estilos inline", {
  for (columns in 1:4) for (mode in c("auto", "stack", "fixed")) {
    row <- racafeFormRow(shiny::tags$input(id = "mod-campo"), columns = columns, responsive = mode)
    html <- as.character(row)
    expect_match(html, "rf-row-container", fixed = TRUE)
    expect_match(html, paste0("rf-row--", mode), fixed = TRUE)
    expect_match(html, paste0("rf-row--", columns), fixed = TRUE)
    expect_match(html, 'id="mod-campo"', fixed = TRUE)
    expect_false(grepl("style=", html, fixed = TRUE))
  }
  expect_error(racafeFormRow(responsive = "invalid"))
  expect_error(racafeFormRow(columns = 5, responsive = "fixed"))
  expect_match(as.character(racafeFormRow("Campo", "Botón", actions = TRUE, responsive = "fixed")),
               "rf-row--actions", fixed = TRUE)
})

test_that("los ejemplos de galería cubren todas las modalidades y columnas con namespaces", {
  for (columns in 1:4) for (mode in c("auto", "stack", "fixed", "legacy")) {
    ns <- shiny::NS("test")
    code <- racafeForms:::.rf_gallery_rows_code(columns, mode)
    html <- as.character(eval(parse(text = code)))
    expect_match(html, 'id="test-FT_LinNeg"', fixed = TRUE)
    expect_match(html, 'id="test-FT_Producto"', fixed = TRUE)
    expect_match(html, paste0("rf-row--", columns), fixed = TRUE)
    expect_identical(grepl("rf-row-container", html, fixed = TRUE), mode != "legacy")
  }
})
