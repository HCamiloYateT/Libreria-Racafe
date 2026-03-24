library(testthat)
library(racafeForecast)

# ============================================================
# Tests — racafeForecast: imputacion, pronosticos y metricas
# ============================================================

# ---- aplicar_imputacion ----

test_that("promedio imputa con la media correcta", {
  serie    <- c(1, 2, NA, 4, 5)
  resultado <- aplicar_imputacion(serie, "promedio")
  expect_false(any(is.na(resultado)))
  expect_equal(resultado[3], mean(c(1, 2, 4, 5)))
})

test_that("mediana imputa con la mediana correcta", {
  serie    <- c(1, 2, NA, 4, 5)
  resultado <- aplicar_imputacion(serie, "mediana")
  expect_equal(resultado[3], stats::median(c(1, 2, 4, 5)))
})

test_that("constante imputa con el valor especificado", {
  serie    <- c(1, NA, 3)
  resultado <- aplicar_imputacion(serie, "constante", valor_constante = 99)
  expect_equal(resultado[2], 99)
})

test_that("constante sin valor_constante genera error", {
  expect_error(aplicar_imputacion(c(1, NA), "constante"))
})

test_that("metodo invalido genera error claro", {
  expect_error(
    aplicar_imputacion(c(1, NA), "metodo_inventado"),
    "metodo_inventado"
  )
})

test_that("serie sin NA retorna original sin modificar", {
  serie <- c(10, 20, 30, 40)
  expect_equal(aplicar_imputacion(serie, "promedio"), serie)
})

test_that("interpolacion requiere zoo", {
  skip_if_not_installed("zoo")
  serie    <- c(0, NA, NA, 6)
  resultado <- aplicar_imputacion(serie, "interpolacion")
  expect_false(any(is.na(resultado)))
  expect_equal(resultado[2], 2)
  expect_equal(resultado[3], 4)
})

# ---- extraer_intervalos ----

test_that("extraer_intervalos falla con nivel no disponible", {
  skip_if_not_installed("forecast")
  ts_d <- stats::ts(rnorm(24), frequency = 12)
  mod  <- forecast::ets(ts_d)
  fc   <- forecast::forecast(mod, h = 6, level = c(80, 95))
  # Nivel 99 no esta en el objeto
  expect_error(extraer_intervalos(fc, 0.99))
})

test_that("extraer_intervalos retorna tibble con columnas lower/upper", {
  skip_if_not_installed("forecast")
  ts_d <- stats::ts(rnorm(24), frequency = 12)
  mod  <- forecast::ets(ts_d)
  fc   <- forecast::forecast(mod, h = 6, level = c(80, 95))
  res  <- extraer_intervalos(fc, 0.95)
  expect_true(is.data.frame(res))
  expect_true("lower" %in% names(res))
  expect_true("upper" %in% names(res))
  expect_equal(nrow(res), 6)
})

# ---- Pronosticar ----

test_that("Pronosticar falla sin columnas numericas", {
  df <- data.frame(
    fecha = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    texto = rep("x", 24)
  )
  expect_error(Pronosticar(df, fecha_col = "fecha"))
})

test_that("Pronosticar falla con prop_train fuera de (0,1)", {
  df <- data.frame(
    fecha  = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    ventas = rnorm(24, 100, 10)
  )
  expect_error(Pronosticar(df, prop_train = 0))
  expect_error(Pronosticar(df, prop_train = 1.1))
})

test_that("Pronosticar falla con fecha_col inexistente", {
  df <- data.frame(fecha = Sys.Date(), val = 1)
  expect_error(Pronosticar(df, fecha_col = "no_existe"))
})

test_that("Pronosticar retorna objeto pronostico_racafe", {
  skip_if_not_installed("forecast")
  set.seed(42)
  df <- data.frame(
    fecha  = seq(as.Date("2019-01-01"), by = "month", length.out = 36),
    ventas = cumsum(rnorm(36, 100, 10))
  )
  pron <- Pronosticar(df, "fecha", "ventas", h_periods = 6)
  expect_true(inherits(pron, "pronostico_racafe"))
  expect_true("resultados" %in% names(pron))
  expect_true("ventas" %in% pron$columnas)
})

# ---- PronMetricas ----

test_that("PronMetricas falla con objeto que no es pronostico_racafe", {
  expect_error(PronMetricas(list(x = 1)), "pronostico_racafe")
})

# ---- .generar_fechas_futuras ----

test_that(".generar_fechas_futuras genera h fechas mensuales", {
  fechas <- seq(as.Date("2023-01-01"), by = "month", length.out = 24)
  futuras <- racafeForecast:::.generar_fechas_futuras(fechas, 6)
  expect_length(futuras, 6)
  expect_true(all(futuras > max(fechas)))
})

# ---- .calcular_metricas ----

test_that(".calcular_metricas calcula RMSE correctamente", {
  real <- c(10, 20, 30, 40, 50)
  pred <- c(12, 18, 32, 38, 52)
  res  <- racafeForecast:::.calcular_metricas(real, pred)
  expect_equal(res$rmse, sqrt(mean((real - pred)^2)))
  expect_true(res$mae  > 0)
  expect_true(res$mape > 0)
})

test_that(".calcular_metricas maneja ceros en denominador de MAPE", {
  real <- c(0, 10, 20)
  pred <- c(1, 11, 21)
  res  <- racafeForecast:::.calcular_metricas(real, pred)
  expect_true(!is.nan(res$mape))
})
