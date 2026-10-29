test_that("los doce preloaders tienen contenido seguro y recursos locales", {
  for (type in c("inicio", "actualizar", "consultar", "calculando", "guardar", "enviar",
                 "generar", "descargar", "validar", "importar", "eliminar", "sincronizar")) {
    preset <- racafePreloader(type)
    expect_named(preset, c("html", "color"))
    expect_match(as.character(preset$html), paste0("rsh-loader--", type))
    expect_match(as.character(preset$html), 'role="status"', fixed = TRUE)
    expect_false(grepl("https://", as.character(preset$html), fixed = TRUE))
    expect_length(htmltools::htmlDependencies(preset$html), 1L)
    expect_match(as.character(preset$html), 'src="racafe-shiny-loaders-1.2.0/logo2.png"', fixed = TRUE)
    expect_match(as.character(preset$html), .rsh_loader_messages[[type]], fixed = TRUE)
  }
  html <- as.character(racafePreloader(text = htmltools::HTML("<script>alert(1)</script>"))$html)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
  quoted <- as.character(racafePreloader(text = "L'analyse")$html)
  expect_match(quoted, "L&#39;analyse", fixed = TRUE)
  expect_error(racafePreloader("otro"))
  expect_error(racafePreloader(text = NA_character_))
  expect_error(racafePreloader(logo = character()))
})

test_that("la UI aplica namespace una vez y conserva los IDs", {
  ns <- shiny::NS("modulo")
  ui <- as.character(racafeLoaderUI(triggers = c(actualizar = "recargar", calculando = "modulo-calcular"), ns = ns))
  expect_match(ui, 'data-rsh-trigger="modulo-recargar"', fixed = TRUE)
  expect_match(ui, 'data-rsh-trigger="modulo-calcular"', fixed = TRUE)
  expect_false(grepl("modulo-modulo", ui, fixed = TRUE))
  expect_match(ui, 'data-rsh-type="actualizar"', fixed = TRUE)
  expect_match(as.character(racafeLoaderUI(startup = FALSE, busy = FALSE)), 'data-busy="false"', fixed = TRUE)
  expect_error(racafeLoaderUI(startup = NA))
  expect_error(racafeLoaderUI(busy = 1))
  expect_error(racafeLoaderUI(triggers = c(otro = "id")))
  expect_error(racafeLoaderUI(triggers = NA_character_))
  expect_error(racafeLoaderUI(ns = "modulo"))
})

test_that("los objetos waiter se aíslan por sesión y namespace", {
  messages <- list()
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  session$sendCustomMessage <- function(type, message) {
    messages[[length(messages) + 1L]] <<- list(type = type, message = message)
  }
  scope <- session$makeScope("modulo")
  loader <- racafeLoader(scope, "actualizar", id = "panel")
  expect_s3_class(loader, "racafe_loader")
  loader$show()
  expect_equal(messages[[1]]$type, "waiter-show")
  expect_equal(messages[[1]]$message$id, "modulo-panel")
  expect_false(messages[[1]]$message$hideOnRender)
  loader$update(racafePreloader(text = "Nuevo")$html)
  expect_equal(messages[[2]]$type, "waiter-update")
  expect_match(messages[[2]]$message$html, "Nuevo")
  loader$hide()
  expect_equal(messages[[3]]$type, "waiter-hide")
  expect_equal(messages[[3]]$message$id, "modulo-panel")
  racafeLoader(scope, id = "modulo-panel")$show()
  expect_equal(messages[[4]]$message$id, "modulo-panel")
  racafeLoader(scope)$show()
  expect_null(messages[[5]]$message$id)
  other <- shiny::MockShinySession$new()
  on.exit(other$close(), add = TRUE)
  shiny::withReactiveDomain(other, loader$hide())
  expect_equal(messages[[6]]$message$id, "modulo-panel")
  expect_error(racafeLoader(session = NULL))
  expect_error(racafeLoader(scope, id = NA_character_))
})


test_that("el logo predeterminado se puede reemplazar u ocultar explícitamente", {
  expect_false(grepl("<img", as.character(racafePreloader(show_logo = FALSE)$html), fixed = TRUE))
  custom <- as.character(racafePreloader(logo = "img/otro.png")$html)
  expect_match(custom, 'src="img/otro.png"', fixed = TRUE)
  expect_match(custom, 'data-rsh-fallback="racafe-shiny-loaders-1.2.0/logo2.png"', fixed = TRUE)
  expect_false(grepl("<img", as.character(racafeLoaderUI(show_logo = FALSE)), fixed = TRUE))
  expect_error(racafePreloader(show_logo = NA))
  expect_error(racafeLoaderUI(show_logo = 1))
})

test_that("el registro admite operaciones repetidas y todos los tipos", {
  ui <- as.character(racafeLoaderUI(triggers = c(enviar = "correo", enviar = "solicitud", guardar = "crear")))
  expect_match(ui, 'data-rsh-trigger="correo" data-rsh-type="enviar"', fixed = TRUE)
  expect_match(ui, 'data-rsh-trigger="solicitud" data-rsh-type="enviar"', fixed = TRUE)
  for (type in .rsh_loader_types()) expect_match(ui, paste0('data-rsh-type="', type, '"'), fixed = TRUE)
  expect_error(racafeLoaderUI(triggers = stats::setNames("id", NA_character_)))
  # Posiciones públicas previas preservadas; show_logo se añade al final.
  expect_equal(names(formals(racafePreloader))[1:3], c("type", "text", "logo"))
  expect_equal(names(formals(racafeLoader))[1:6], c("session", "type", "text", "logo", "id", "ns"))
})

test_that("cada operación se puede usar en un objeto con logo u ocultarlo", {
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  last <- NULL
  session$sendCustomMessage <- function(type, message) last <<- message
  for (type in .rsh_loader_types()) {
    racafeLoader(session, type = type)$show()
    expect_match(last$html, paste0("rsh-loader--", type), fixed = TRUE)
    expect_match(last$html, "logo2.png", fixed = TRUE)
  }
  racafeLoader(session, type = "guardar", show_logo = FALSE)$show()
  expect_false(grepl("<img", last$html, fixed = TRUE))
})
