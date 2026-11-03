# Modales con estilos precargados y botones nativos ----
test_that("los tamaños están disponibles en las dependencias iniciales", {
  dep <- racafeFormsDependencies()
  expect_true("racafe-modal-sizes.css" %in% dep$stylesheet)
  css <- readLines(system.file("www", "racafe-modal-sizes.css", package = "racafeForms"))
  expect_true(any(grepl("--rf-modal-sizes-ready:1", css, fixed = TRUE)))
  for (size in c("ventana", paste0("subventana", 1:5), "aviso")) {
    expect_true(any(grepl(paste0(".rf-modal.", size, " .modal-dialog"), css, fixed = TRUE)))
    html <- as.character(racafeModal("Texto", size = size))
    expect_match(html, "--rf-forms-ready", fixed = TRUE)
    expect_match(html, "--rf-modal-sizes-ready", fixed = TRUE)
    expect_match(html, "requestAnimationFrame(ready)", fixed = TRUE)
  }
})
test_that("footers normalizan cancelar y confirmar sin alterar identidad ni handlers", {
  tag <- racafeModalFooter(
    shiny::modalButton("Cancelar"), shiny::actionButton("padre-confirmar", "Confirmar"),
    left = shiny::actionButton("padre-help", "Ayuda"))
  buttons <- list()
  walk <- function(node) {
    if (inherits(node, "shiny.tag")) {
      if (node$name == "button") buttons[[length(buttons) + 1L]] <<- node
      lapply(node$children, walk)
    } else if (is.list(node)) lapply(node, walk)
  }
  invisible(walk(tag))
  expect_length(buttons, 3L)
  expect_true(all(vapply(buttons, function(x) "rf-btn" %in% strsplit(x$attribs$class, " ")[[1]], logical(1))))
  confirm <- Filter(function(x) identical(x$attribs$id, "padre-confirmar"), buttons)[[1]]
  cancel <- Filter(function(x) identical(x$attribs$`data-dismiss`, "modal"), buttons)[[1]]
  expect_match(confirm$attribs$class, "rf-btn--primary", fixed = TRUE)
  expect_match(confirm$attribs$class, "action-button", fixed = TRUE)
  expect_match(cancel$attribs$class, "rf-btn--secondary", fixed = TRUE)
  expect_match(as.character(racafeModal(footer = shiny::modalButton("Cerrar"))), "rf-modal-footer", fixed = TRUE)
  expect_false(grepl("modal-footer", as.character(racafeModal(footer = NULL)), fixed = TRUE))
})
