# Archivos y metadatos ----
test_that("fileInput conserva estructura, namespace y opciones sin reemplazar el binding", {
  tag <- racafeInputFile("archivo", accept = c(".xlsx", ".pdf"), multiple = TRUE, max_size = 1024,
                         help = "Ayuda", disabled = TRUE, ns = shiny::NS("m"))
  input <- htmltools::tagQuery(tag)$find(".shiny-input-file")$selectedTags()[[1]]
  expect_identical(input$attribs$id, "m-archivo")
  expect_match(input$attribs$class, "shiny-input-file", fixed = TRUE)
  expect_identical(input$attribs$disabled, "disabled")
  expect_identical(input$attribs[["data-rf-max-size"]], 1024)
  expect_match(input$attribs[["aria-describedby"]], "m-archivo_error", fixed = TRUE)
  html <- as.character(tag)
  for (text in c("rf-file--compact", "m-archivo_progress", "m-archivo_files", "multiple", "readonly=", "Seleccionar")) {
    expect_match(html, text, fixed = TRUE)
  }
  expect_false(grepl("Browse", html, fixed = TRUE))
  expect_match(as.character(racafeInputFile("x", size = "normal")), "rf-file--normal", fixed = TRUE)
  expect_error(racafeInputFile("x", multiple = NA), "logico|lógico")
  expect_error(racafeInputFile("x", accept = "csv"), "accept")
  for (max in list(-1, 0, Inf, NA, 1.2, "100", c(1, 2))) expect_error(racafeInputFile("x", max_size = max), "max_size")
})

test_that("validación servidor comprueba límites reales, extensión, vacío y lectura", {
  path <- tempfile()
  on.exit(unlink(path))
  writeBin(charToRaw("a,b\n1,2\n"), path)
  file <- data.frame(name = "PRUEBA.CSV", size = file.info(path)$size, type = "text/csv", datapath = path)
  expect_true(racafeValidateInputFile(file, accept = ".csv", max_size = 8)$valid)
  expect_true(racafeValidateInputFile(file, accept = "text/*")$valid)
  expect_true(racafeValidateInputFile(file, accept = "text/csv")$valid)
  expect_false(racafeValidateInputFile(file, accept = ".pdf")$valid)
  expect_false(racafeValidateInputFile(file, max_size = 7)$valid)
  expect_true(racafeValidateInputFile(rbind(file, file), accept = ".csv")$valid)
  file$size <- 1
  expect_false(racafeValidateInputFile(file, max_size = 2)$valid)
  expect_match(paste(racafeValidateInputFile(file)$errors, collapse = " "), "real", fixed = TRUE)
  file$datapath <- paste0(path, "-ausente")
  expect_match(paste(racafeValidateInputFile(file)$errors, collapse = " "), "leer", fixed = TRUE)
  writeBin(raw(), path)
  file$datapath <- path
  file$size <- 0
  expect_match(paste(racafeValidateInputFile(file)$errors, collapse = " "), "vacio", fixed = TRUE)
  expect_false(racafeValidateInputFile(NULL)$valid)
  expect_false(racafeValidateInputFile(list())$valid)
  file$size <- "8"
  expect_false(racafeValidateInputFile(file)$valid)
})

# Notificaciones nativas ----
test_that("notify usa segundos, escapa HTML, reutiliza IDs y deja SweetAlert separado", {
  session <- shiny::MockShinySession$new()
  on.exit(session$close())
  calls <- list()
  session$sendNotification <- function(type, message) calls[[length(calls) + 1L]] <<- list(type, message)
  id <- racafeNotify(session, htmltools::HTML('<script>titulo</script>'),
                     htmltools::HTML('<img src=x>'), id = "operacion")
  message <- calls[[1]][[2]]
  expect_identical(calls[[1]][[1]], "show")
  expect_identical(message$type, "default")
  expect_equal(message$duration, 5000)
  expect_match(message$html, "&lt;script&gt;", fixed = TRUE)
  expect_match(message$html, "&lt;img", fixed = TRUE)
  expect_false(grepl("<script>|<img |swal", message$html))
  expect_identical(racafeNotify(session, "Actualizar", id = "operacion", duration = NULL), id)
  expect_identical(racafeNotify(session, "Actualizar", id = id, duration = 1), id)
  expect_null(calls[[2]][[2]]$duration)
  expect_equal(calls[[3]][[2]]$duration, 1000)
  for (type in c("info", "success", "warning", "error")) {
    racafeNotify(session, type, type = type, close_button = FALSE)
    expect_match(tail(calls, 1)[[1]][[2]]$html, paste0("rf-notify--", type), fixed = TRUE)
    expect_false(tail(calls, 1)[[1]][[2]]$closeButton)
  }
  expect_identical(racafeRemoveNotify(id, session), id)
  expect_identical(tail(calls, 1)[[1]], list("remove", id))
  for (duration in list(0, -1, NA, Inf, "5", c(1, 2))) expect_error(racafeNotify(session, "X", duration = duration))
  expect_error(racafeNotify(session, "X", id = 'x" onclick="bad'), "id")
})

test_that("IDs propios no reemplazan notificaciones nativas y reciben namespace una vez", {
  session <- shiny::MockShinySession$new()
  on.exit(session$close())
  session$sendNotification <- function(...) NULL
  session <- session$makeScope("modulo")
  id <- racafeNotify(session, "Titulo", id = "nativo")
  expect_identical(id, "modulo-rf-notify-key-nativo")
  expect_identical(racafeNotify(session, "Otro", id = id), id)
  a <- racafeNotify(session, "A")
  b <- racafeNotify(session, "B")
  expect_false(identical(a, b))
  expect_match(a, "modulo-rf-notify-auto-", fixed = TRUE)
})
