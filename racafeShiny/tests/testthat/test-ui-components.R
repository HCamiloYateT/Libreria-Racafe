test_that("el contenedor es semántico, compacto y escapa títulos", {
  tag <- racafeCollapsible("detalle", "<script>no</script>", TRUE, shiny::p("Contenido"), icon = "list")
  html <- as.character(tag)
  expect_match(html, "<details", fixed = TRUE)
  expect_match(html, '<summary aria-controls="detalle_content" aria-expanded="true">', fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_match(html, "Contenido", fixed = TRUE)
  expect_match(html, "--rsh-collapse-color:#DA291C", fixed = TRUE)
  expect_length(htmltools::htmlDependencies(tag), 1L)
  expect_false(grepl('open="open"', as.character(racafeCollapsible("cerrado", "Título")), fixed = TRUE))
  expect_match(as.character(racafeCollapsible("gris", "Título", color = "gray30")),
               "--rsh-collapse-color:#4D4D4D", fixed = TRUE)
  expect_error(racafeCollapsible("id", "Título", open = NA))
  expect_error(racafeCollapsible("id", "Título", color = "red;display:none"))
  expect_error(racafeCollapsible("id", "Título", icon = 123))
})

test_that("update conserva IDs locales y no duplica namespaces", {
  received <- list()
  session <- list(ns = shiny::NS("padre-hijo"), sendInputMessage = function(id, message) {
    received[[length(received) + 1L]] <<- list(id = id, message = message)
  })
  racafeUpdateCollapsible(session, "detalle", TRUE)
  racafeUpdateCollapsible(session, "padre-hijo-detalle", FALSE)
  expect_equal(received[[1]], list(id = "detalle", message = list(open = TRUE)))
  expect_equal(received[[2]], list(id = "detalle", message = list(open = FALSE)))
  expect_error(racafeUpdateCollapsible(NULL, "id", TRUE))
  expect_error(racafeUpdateCollapsible(session, "id", 1))
})

test_that("el footer reutiliza botones nativos y no modifica sus IDs", {
  skip_if_not_installed("racafeForms")
  button <- racafeForms::racafeButtonSave("modulo-guardar", "Guardar")
  html <- htmltools::renderTags(racafeModalFooter(acciones = button))$html
  expect_match(html, 'class="rsh-modal-footer"', fixed = TRUE)
  expect_match(html, 'id="modulo-guardar"', fixed = TRUE)
  expect_match(html, 'data-dismiss="modal"', fixed = TRUE)
  expect_match(html, "rf-btn--primary", fixed = TRUE)
  expect_match(html, "rf-btn--secondary", fixed = TRUE)
  no_close <- as.character(racafeModalFooter(cerrar = FALSE, acciones = button))
  expect_false(grepl('data-dismiss="modal"', no_close, fixed = TRUE))
  expect_match(as.character(racafeModalFooter(texto_cerrar = "Cancelar")), "Cancelar", fixed = TRUE)
  expect_error(racafeModalFooter(cerrar = NA))
})


test_that("el badge es hermano del título, conserva outputs y no interpreta HTML del título", {
  tag <- racafeCollapsible("padre-lista", "<b>Cuentas</b>", open = TRUE, shiny::p("Detalle"),
    header_badge = shiny::uiOutput("padre-conteo", inline = TRUE))
  summary <- tag$children[[1]]
  html <- as.character(summary)
  expect_match(html, "&lt;b&gt;Cuentas&lt;/b&gt;", fixed = TRUE)
  expect_match(html, 'class="rsh-collapsible-header-badge"', fixed = TRUE)
  expect_match(html, 'id="padre-conteo"', fixed = TRUE)
  expect_match(html, 'aria-controls="padre-lista_content"', fixed = TRUE)
  expect_match(html, 'aria-expanded="true"', fixed = TRUE)
  expect_false(grepl("Detalle", html, fixed = TRUE))
  expect_equal(tail(names(formals(racafeCollapsible)), 1L), "header_badge")
  legacy <- as.character(racafeCollapsible("antes", "Título", TRUE, shiny::p("Detalle")))
  expect_false(grepl("rsh-collapsible-header-badge", legacy, fixed = TRUE))
  expect_match(legacy, "Detalle", fixed = TRUE)
})
