library(testthat)

test_that("Flujo E2E core -> bd/drive -> graph/shiny -> forecast funciona con stubs", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("plotly")
  skip_if_not_installed("forecast")

  toy <- data.frame(
    fecha = seq(as.Date("2024-01-01"), by = "month", length.out = 18),
    categoria = rep(c("A", "B", "C"), length.out = 18),
    valor = c(100, 120, 95, 110, 150, 130, 125, 90, 105, 140, 170, 145, 155, 160, 180, 175, 165, 190)
  )

  # 1) core
  core_df <- racafeCore::TopAbsoluto(
    toy,
    var_recode = categoria,
    var_top = valor,
    fun_Top = "sum",
    n = 2,
    nom_var = "categoria_top"
  )
  expect_true("categoria_top" %in% names(core_df))

  # 2) bd (stub de lectura/escritura sin conexión real)
  mock_db <- new.env(parent = emptyenv())
  mock_db$tabla <- NULL

  stub_escribir <- function(df, tabla, bd = "mock") {
    expect_equal(tabla, "toy_tabla")
    mock_db$tabla <- df
    invisible(TRUE)
  }
  stub_cargar <- function(tabla, condicion = NULL, bd = "mock") {
    expect_equal(tabla, "toy_tabla")
    mock_db$tabla
  }

  expect_true(stub_escribir(core_df, "toy_tabla"))
  bd_df <- stub_cargar("toy_tabla")
  expect_equal(nrow(bd_df), nrow(core_df))

  # 3) drive (stub de API externa robusto)
  stub_listar_carpetas <- function(usuario) {
    expect_equal(usuario, "svc.racafe")
    data.frame(
      nombre = c("insumos", "salidas"),
      id = c("id-insumos", "id-salidas"),
      es_carpeta = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    )
  }
  drive_items <- stub_listar_carpetas("svc.racafe")
  expect_true(all(c("nombre", "id", "es_carpeta") %in% names(drive_items)))

  # 4) graph/shiny
  p <- racafeGraph::ImprimirAnillo(
    bd_df,
    var_label = "categoria_top",
    var_medida = "valor",
    funcion = "sum"
  )
  expect_true(inherits(p, "plotly"))

  box_ui <- racafeShiny::CajaValor(
    valor = sum(bd_df$valor),
    formato = "numero",
    texto = "Valor total",
    icono = "chart-line",
    mostrar_boton = FALSE
  )
  expect_true(inherits(box_ui, "shiny.tag"))

  # 5) forecast
  pron <- racafeForecast::Pronosticar(
    bd_df[, c("fecha", "valor")],
    fecha_col = "fecha",
    valor_cols = "valor",
    h_periods = 3,
    prop_train = 0.8,
    nivel_confianza = 0.95,
    metodo_imputacion = "interpolacion"
  )

  expect_true(inherits(pron, "pronostico_racafe"))
  expect_true("resultados" %in% names(pron))
  expect_true(nrow(pron$resultados) > 0)
})
