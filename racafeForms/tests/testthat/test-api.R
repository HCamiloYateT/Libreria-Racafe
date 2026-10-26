test_that("IDs locales y namespace", {
  expect_identical(racafeForms:::`.rf_id`("x"), "x")
  expect_identical(racafeForms:::`.rf_id`("x", function(x) paste0("mod-", x)), "mod-x")
})
test_that("formatos invalidos fallan", {
  expect_error(racafeInputNumeric("n", "Numero", format = "inexistente"), "Formato")
})
test_that("botones requieren contenido", {
  expect_error(racafeButton("btn", NULL), "label")
})
test_that("contenedor y opciones renderizan HTML", {
  html <- as.character(racafeForm(racafeInputSelect("asesor", "Asesor", c("A", "B"))))
  expect_match(html, "rf-form", fixed = TRUE)
  expect_match(html, "asesor", fixed = TRUE)
})


test_that("la dependencia CSS corresponde al paquete instalado", {
  dep <- racafeFormsDependencies()
  expect_identical(dep$version, as.character(utils::packageVersion("racafeForms")))
  expect_true(file.exists(system.file(dep$src$file, dep$stylesheet, package = dep$package)))
})
