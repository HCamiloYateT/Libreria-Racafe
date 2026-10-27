# Selectores y rangos ----
test_that("el selector simple conserva etiquetas, valores y opción vacía", {
  html <- as.character(racafeInputSelect("periodo", "Periodo", c(Dinámico = "D", Estático = "E")))
  expect_match(html, 'value="" selected', fixed = TRUE)
  expect_match(html, 'value="D">Dinámico', fixed = TRUE)
  expect_match(as.character(racafeInputSelect("periodo", choices = c(A = "a"), selected = "a")),
               'value="a" selected', fixed = TRUE)
  expect_false(grepl('value=""', as.character(racafeInputSelect("x", choices = "a", empty = FALSE)), fixed = TRUE))
})

test_that("el selector múltiple configura resumen, género y namespace sin opción artificial", {
  for (gender in c("m", "f")) {
    for (selected in list(character(), "a", c("a", "b"), c("a", "b", "c"))) {
      html <- as.character(racafeInputSelect("asesor", choices = c(Ana = "a", Luis = "b", Marta = "c"),
                                            multiple = TRUE, selected = selected, gender = gender, ns = shiny::NS("filtro")))
      expect_match(html, 'id="filtro-asesor"', fixed = TRUE)
      expect_match(html, 'data-selected-text-format="count &gt; 1"', fixed = TRUE)
      expect_match(html, if (gender == "f") 'data-none-selected-text="Ninguna"' else 'data-none-selected-text="Ninguno"',
                   fixed = TRUE)
      expect_false(grepl('<option value=""', html, fixed = TRUE))
    }
  }
  expect_error(racafeInputSelect("x", choices = "a", gender = "otro"), "arg")
  html <- as.character(racafeInputSelect("x", choices = character(), multiple = TRUE, gender = "f"))
  expect_match(html, 'data-rf-gender="f"', fixed = TRUE)
  expect_match(as.character(racafeInputSelect("x", choices = "a", gender = "f", empty_label = "Sin datos")),
               'data-none-selected-text="Sin datos"', fixed = TRUE)
})

test_that("actualizar opciones conserva únicamente selecciones válidas y respeta el vacío explícito", {
  mensaje <- NULL
  session <- list(input = list(asesor = c("a", "c")),
                  sendInputMessage = function(id, message) mensaje <<- list(id = id, message = message))
  racafeUpdateSelect(session, "asesor", choices = c(Ana = "a", Luis = "b"))
  expect_identical(mensaje$id, "asesor")
  expect_identical(mensaje$message$value, "a")
  racafeUpdateSelect(session, "asesor", choices = c(Ana = "a"), selected = character())
  expect_identical(mensaje$message$value, character())
  session$input$asesor <- NULL
  racafeUpdateSelect(session, "asesor", choices = c(Ana = "a", Luis = "b"))
  expect_identical(mensaje$message$value, character())
  racafeUpdateSelect(session, "asesor", selected = "b")
  expect_identical(mensaje$message$value, "b")
  session$input$asesor <- c("a", "b")
  racafeUpdateSelect(session, "asesor", choices = list(Grupo = c(Ana = "a")))
  expect_identical(mensaje$message$value, "a")
})

test_that("el rango conserva binding, límites, idioma y namespace", {
  html <- as.character(racafeInputDateRange("fecha", "Fecha", start = "2026-01-01", end = "2026-10-26",
                                           min = "2026-01-01", max = "2026-12-31", ns = shiny::NS("filtro")))
  for (text in c('id="filtro-fecha"', 'shiny-date-range-input', 'rf-date-range', 'data-date-language="es"',
                 'data-initial-date="2026-01-01"', 'data-initial-date="2026-10-26"',
                 'data-min-date="2026-01-01"', 'data-max-date="2026-12-31"', 'dd/mm/yyyy')) {
    expect_match(html, text, fixed = TRUE)
  }
  expect_match(html, 'class="input-group-text">-</span>', fixed = TRUE)
  mensaje <- NULL
  session <- shiny::MockShinySession$new()
  on.exit(session$close())
  session$sendInputMessage <- function(id, message) mensaje <<- list(id = id, message = message)
  racafeUpdateDateRange(session, "fecha", start = as.Date("2026-02-01"), end = as.Date("2026-03-01"))
  expect_identical(mensaje$id, "fecha")
  expect_identical(mensaje$message$value$start, "2026-02-01")
  expect_identical(mensaje$message$value$end, "2026-03-01")
})

test_that("los recursos se adjuntan incluso sin un contenedor racafeForm", {
  for (tag in list(racafeInputSelect("x", choices = "a"), racafeInputDateRange("r"))) {
    deps <- htmltools::htmlDependencies(tag)
    dep <- Filter(function(x) is.list(x) && identical(x$name, "racafeForms"), deps)
    expect_length(dep, 1)
    expect_identical(dep[[1]]$script, "racafe-forms.js")
  }
})

test_that("fecha y rango comparten el inicio de semana y recursos propios", {
  fecha <- racafeInputDate("fecha", value = "2026-10-15", min = "2026-01-01", max = "2026-12-31",
                           ns = shiny::NS("filtro"))
  html <- as.character(fecha)
  for (text in c('id="filtro-fecha"', 'rf-date', '"firstDay":1', '"minDate":"2026-01-01"',
                 '"maxDate":"2026-12-31"', '"dateFormat":"dd/MM/yyyy"', '"language":"ES"')) {
    expect_match(html, text, fixed = TRUE)
  }
  expect_false(grepl('dp-addon', html, fixed = TRUE))
  deps <- htmltools::htmlDependencies(fecha)
  expect_true(any(vapply(deps, function(x) is.list(x) && identical(x$name, "racafeForms"), logical(1))))
  rango <- as.character(racafeInputDateRange("rango"))
  expect_match(rango, 'data-date-week-start="1"', fixed = TRUE)
})
