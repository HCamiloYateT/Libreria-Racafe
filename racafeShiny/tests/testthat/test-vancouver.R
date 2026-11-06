test_that("Vancouver conserva datos y devuelve una tabla gt componible", {
  data <- data.frame(Producto = c("Café <script>texto</script>", " Total "), Cantidad = c(2, 2),
    Total = c(1234.56, 1234.56), check.names = FALSE)
  original <- data
  table <- racafeTableVancouver(data, min_width = "800px", max_height = "300px", labels = c(Cantidad = "Unidades"))
  expect_s3_class(table, "gt_tbl")
  expect_identical(data, original)
  html <- as.character(htmltools::as.tags(gt::tab_source_note(table, "Nota añadida")))
  expect_match(html, "1,234.56", fixed = TRUE)
  expect_match(html, "Unidades", fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>texto", html, fixed = TRUE))
  expect_match(html, "min-width:800px", fixed = TRUE)
  expect_match(html, "max-height:300px", fixed = TRUE)
  expect_match(html, "Nota añadida", fixed = TRUE)
})

test_that("Vancouver cubre vacíos, NA y totales configurables", {
  expect_match(as.character(htmltools::as.tags(racafeTableVancouver(data.frame(Item = character(), Total = numeric())))),
    "Sin registros", fixed = TRUE)
  data <- data.frame(Item = c(NA, "tOtAl"), Importe = c(NA, 2))
  expect_s3_class(racafeTableVancouver(data, total_rows = FALSE, total_columns = FALSE), "gt_tbl")
  expect_s3_class(racafeTableVancouver(data, labels = c(Importe = "Total"), decimals = NULL,
    align = c(Importe = "center"), title = "Café & Cía.", subtitle = "Prueba"), "gt_tbl")
  expect_error(racafeTableVancouver(data, align = c(Otra = "left")), "align")
  expect_error(racafeTableVancouver(data, labels = c(Otra = "Total")), "labels")
  expect_error(racafeTableVancouver(data, header_width = -1), "tama")
  expect_error(racafeTableVancouver(data, decimals = 1.5), "decimals")
  expect_error(racafeTableVancouver(data, total_rows = NA), "estado")
  expect_error(racafeTableVancouver(data, total_background = "red;}body{color:red"), "Color")
})

test_that("El reconocimiento de columna Total sigue la etiqueta visible", {
  data <- data.frame(Item = "Café", Total = 2)
  html <- gt::as_raw_html(racafeTableVancouver(data, labels = c(Total = "Importe"), total_rows = FALSE))
  expect_false(grepl("#F5F6F7", html, fixed = TRUE))
})
