test_that("los formatos usan punto decimal y coma de miles", {
  expect_equal(DefinirFormato("numero")(1234.56), "1,234.56")
  expect_equal(DefinirFormato("dolares")(1234.56), "$1,234.56")
  expect_equal(DefinirFormato("porcentaje")(12.3456), "1,234.56%")
})

test_that("las tablas gt usan punto decimal y coma de miles", {
  skip_if_not_installed("gt")
  table <- gt::gt(data.frame(valor = 12.345))
  expect_match(gt::as_raw_html(gt_pct_style(table, valor)), "1,234.5%", fixed = TRUE)
})
