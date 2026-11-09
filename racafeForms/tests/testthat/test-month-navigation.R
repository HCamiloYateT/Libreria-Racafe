test_that("el selector mensual normaliza fechas y conserva namespace", {
  html <- htmltools::renderTags(racafeInputMonth("mes", value = as.Date("2026-08-24"),
    min = as.Date("2026-01-19"), max = as.Date("2026-09-30"), ns = shiny::NS("mod")))$html
  expect_match(html, 'id="mod-mes"', fixed = TRUE)
  expect_match(html, 'months', fixed = TRUE)
  expect_match(html, 'MM/yyyy', fixed = TRUE)
  expect_match(html, '2026-08-01', fixed = TRUE)
  expect_error(racafeInputMonth("mes", min = as.Date("2026-12-01"), max = as.Date("2026-01-01")))
})
test_that("desplegables y pestañas conservan semántica y no duplican namespace", {
  cerrado <- htmltools::renderTags(racafeDisclosure("Guía", shiny::tags$p("Contenido")))$html
  expect_match(cerrado, '<details', fixed = TRUE)
  expect_false(grepl('open=', cerrado, fixed = TRUE))
  tabs <- htmltools::renderTags(racafeTabs(shiny::tabPanel("Uno"), shiny::tabPanel("Dos"),
    id = "vista", ns = shiny::NS("mod")))$html
  expect_match(tabs, 'id="mod-vista"', fixed = TRUE)
  expect_false(grepl('mod-mod-vista', tabs, fixed = TRUE))
})

test_that("la actualización mensual normaliza valor y límites mediante options nativas", {
  enviado <- NULL
  sesion <- list(ns = shiny::NS("mod"), sendInputMessage = function(id, message) enviado <<- message)
  racafeUpdateMonth(sesion, "mes", value = as.Date("2026-08-24"), min = as.Date("2026-01-19"))
  expect_match(enviado$config, "2026-08-01", fixed = TRUE)
  expect_match(enviado$config, "2026-01-01", fixed = TRUE)
})
