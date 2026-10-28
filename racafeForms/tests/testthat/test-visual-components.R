# Ampliaciones visuales sin reglas de negocio ----
test_that("tarjetas e indicadores amplían densidad y encabezado sin acciones implícitas", {
  for (padding in c("compact", "normal", "comfortable")) {
    html <- as.character(racafeCard("Título", "Cuerpo", padding = padding,
                                    header = racafeStatusBadge("Disponible", "success"), footer = "Pie"))
    expect_match(html, paste0("rf-padding--", padding), fixed = TRUE)
    expect_match(html, 'class="rf-card-header"', fixed = TRUE)
    expect_match(html, 'class="rf-card-footer"', fixed = TRUE)
    expect_false(grepl('data-rf-action|tabindex|min-height|<i ', html))
    metric <- as.character(racafeMetric(42, "Título", detail = "Nota", padding = padding))
    expect_match(metric, "rf-metric-value", fixed = TRUE)
    expect_match(metric, paste0("rf-padding--", padding), fixed = TRUE)
    expect_match(metric, "Nota", fixed = TRUE)
  }
  expect_match(as.character(racafePanel("Panel", "Cuerpo", header = "Encabezado")), "rf-panel-header", fixed = TRUE)
  expect_error(racafeCard(padding = "otro"), "arg")
  expect_error(racafeMetric(1, "Uno", padding = "otro"), "arg")
})

test_that("badges son etiquetas escapadas, semánticas y compactas, sin eventos ni región viva", {
  for (variant in c("success", "warning", "danger", "info", "neutral")) {
    for (size in c("xs", "sm", "md")) {
      html <- as.character(racafeStatusBadge('<script>"&', variant, size))
      expect_match(html, paste0("rf-status-badge--", variant), fixed = TRUE)
      expect_match(html, paste0("rf-status-badge--", size), fixed = TRUE)
      expect_match(html, "&lt;script&gt;", fixed = TRUE)
      expect_false(grepl('<script>|style=|role=|data-rf-action|onclick=', html))
    }
  }
  expect_match(as.character(racafeStatusBadge(htmltools::HTML("<b>Texto</b>"))), "&lt;b&gt;", fixed = TRUE)
  expect_match(as.character(racafeStatusBadge(0)), ">0</span>", fixed = TRUE)
  for (value in list(NULL, NA, c("a", "b"), list("a"))) expect_error(racafeStatusBadge(value), "escalar")
  expect_error(racafeStatusBadge("X", "estado_de_negocio"), "arg")
  expect_error(racafeStatusBadge("X", size = "grande"), "arg")
})

test_that("descarga respeta icono NULL, usa un solo icono y conserva binding namespace", {
  for (icon in list(NULL, "download", shiny::icon("file-arrow-down"))) {
    tag <- racafeButtonDownload("archivo", icon = icon, ns = shiny::NS("m"))
    html <- as.character(tag)
    expect_match(html, 'id="m-archivo"', fixed = TRUE)
    expect_match(html, "shiny-download-link", fixed = TRUE)
    expect_equal(length(gregexpr("<i ", html, fixed = TRUE)[[1]][gregexpr("<i ", html, fixed = TRUE)[[1]] > 0]),
                 if (is.null(icon)) 0 else 1)
  }
})

test_that("reactable serializa badges como celdas y deduplica dependencias corporativas", {
  skip_if_not_installed("reactable")
  widget <- reactable::reactable(data.frame(estado = c("Disponible", "Pendiente")),
    columns = list(estado = reactable::colDef(cell = function(value) racafeStatusBadge(value, "info"))))
  rendered <- htmltools::renderTags(htmltools::tagList(racafeCard("Tabla", widget), racafeStatusBadge("Estado")))
  expect_match(rendered$html, "rf-status-badge", fixed = TRUE)
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  expect_equal(sum(vapply(deps, function(x) identical(x$name, "racafeForms"), logical(1))), 1L)
})


test_that("se conserva el orden de los argumentos públicos de racafeForms 0.3.0", {
  reference <- dget(testthat::test_path("..", "fixtures", "public-signatures-0.3.0.R"))
  for (name in names(reference)) {
    actual <- names(formals(getExportedValue("racafeForms", name)))
    expect_identical(head(actual, length(reference[[name]])), reference[[name]], info = name)
  }
})
