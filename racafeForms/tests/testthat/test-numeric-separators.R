test_that("los separadores numéricos predeterminados son estables", {
  html <- as.character(racafeInputNumeric("importe", "Importe", 1234.56))
  expect_match(html, '"decimalCharacter":"."', fixed = TRUE)
  expect_match(html, '"digitGroupSeparator":","', fixed = TRUE)
  custom <- as.character(racafeInputNumeric("importe", "Importe", thousand = ".", decimal = ","))
  expect_match(custom, '"decimalCharacter":","', fixed = TRUE)
  expect_match(custom, '"digitGroupSeparator":"."', fixed = TRUE)
})
