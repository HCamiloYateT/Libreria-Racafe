library(testthat)
library(racafeCore)

# ============================================================
# Tests — Seccion Core: texto, fechas, numericas, transformacion
# ============================================================


# ---- Texto y validacion ----

test_that("LimpiarNombres normaliza espacios y convierte a mayusculas", {
  expect_equal(LimpiarNombres("  camilo   yate  "), "CAMILO YATE")
  expect_equal(LimpiarNombres("TEXTO"), "TEXTO")
  expect_equal(LimpiarNombres("a  b  c"), "A B C")
})

test_that("LimpiarCadena elimina caracteres especiales", {
  expect_equal(LimpiarCadena("Hola!"), "Hola")
  expect_equal(LimpiarCadena("cafe123", rem_numeros = TRUE), "cafe")
  expect_match(LimpiarCadena("Niño", rem_acentos = TRUE), "Nino")
})

test_that("EsVacio detecta NULL, NA y cadena vacia", {
  expect_true(EsVacio(""))
  expect_true(EsVacio(NA))
  expect_true(EsVacio(NULL))
  expect_false(EsVacio("texto"))
  expect_false(EsVacio(0))
})

test_that("EsEmail valida formatos correctos e incorrectos", {
  expect_true(EsEmail("usuario@racafe.com"))
  expect_true(EsEmail("user.name+tag@example.co"))
  expect_false(EsEmail("sin_arroba"))
  expect_false(EsEmail("@dominio.com"))
  expect_false(EsEmail(""))
  expect_false(EsEmail(NA))
})

test_that("EsNumTelefono valida celulares colombianos", {
  expect_true(EsNumTelefono("3123456789"))
  expect_true(EsNumTelefono("3001234567"))
  expect_false(EsNumTelefono("1234567890"))
  expect_false(EsNumTelefono("312345678"))
  expect_false(EsNumTelefono(""))
})

test_that("EsNumero valida numeros positivos", {
  expect_true(EsNumero("12.3"))
  expect_true(EsNumero("100"))
  expect_false(EsNumero("-5"))
  expect_false(EsNumero(""))
  expect_false(EsNumero(NA))
  expect_false(EsNumero("abc"))
})

test_that("UnirCadenas omite NA cuando na.rm = TRUE", {
  resultado <- UnirCadenas("Hola", NA, "Mundo", sep = "-", na.rm = TRUE)
  expect_false(grepl("NA", resultado))
  expect_true(grepl("Hola", resultado))
})

test_that("Unicos retorna valores unicos ordenados", {
  expect_equal(Unicos(c("b", "a", "a", "c")), c("a", "b", "c"))
  expect_equal(Unicos(c(3, 1, 2, 1)), c(1, 2, 3))
})


# ---- Funciones numericas ----

test_that("Variacion calcula correctamente", {
  expect_equal(Variacion(100, 120), 0.2)
  expect_equal(Variacion(100, 80), -0.2)
  expect_true(is.na(Variacion(0, 50)))
  expect_true(is.na(Variacion(NA, 50)))
})

test_that("Moda retorna el valor mas frecuente", {
  expect_equal(Moda(c(1, 2, 2, 3)), 2)
  expect_equal(Moda(c("a", "b", "b", "c")), "b")
  expect_equal(Moda(c(NA, 1, 1, NA), na.rm = TRUE), 1)
})

test_that("RedondearMultiplo redondea al multiplo mas cercano", {
  expect_equal(RedondearMultiplo(17, 5), 15)
  expect_equal(RedondearMultiplo(18, 5), 20)
  expect_equal(RedondearMultiplo(c(13, 17, 22), 5), c(15, 15, 20))
})

test_that("SiError_0 retorna 0 en errores", {
  expect_equal(SiError_0(tryCatch(stop("error"), error = function(e) e)), 0)
  expect_equal(SiError_0(5), 5)
})


# ---- Fechas ----

test_that("PrimerDia retorna el primer dia del mes", {
  resultado <- PrimerDia("2023-10-15")
  expect_equal(resultado, as.Date("2023-10-01"))
})

test_that("PrimerDia funciona para otros periodos", {
  expect_equal(PrimerDia("2023-10-15", "year"),    as.Date("2023-01-01"))
  expect_equal(PrimerDia("2023-10-15", "quarter"), as.Date("2023-10-01"))
})

test_that("EdadCumplida calcula anios correctamente", {
  edad <- EdadCumplida(as.Date("1990-01-01"), as.Date("2023-12-31"))
  expect_equal(edad, 33L)

  # No cumplido aun en el anio
  edad2 <- EdadCumplida(as.Date("1990-12-31"), as.Date("2023-06-01"))
  expect_equal(edad2, 32L)
})


# ---- Transformacion de datos ----

test_that("bind_rows_na omite data.frames vacios", {
  df1 <- data.frame(x = 1:2)
  df2 <- data.frame(x = integer(0))
  df3 <- data.frame(x = 3:4)
  resultado <- bind_rows_na(df1, df2, df3)
  expect_equal(nrow(resultado), 4)
})

test_that("bind_rows_na maneja solo vacios", {
  resultado <- bind_rows_na(
    data.frame(x = integer(0)),
    NULL
  )
  expect_equal(nrow(resultado), 0)
})

test_that("left_join_all encadena joins correctamente", {
  base   <- data.frame(id = 1:3)
  extra1 <- data.frame(id = 1:2, a = c("x", "y"))
  extra2 <- data.frame(id = 2:3, b = c("p", "q"))
  resultado <- left_join_all(base, list(extra1, extra2), by = "id")
  expect_equal(nrow(resultado), 3)
  expect_true("a" %in% names(resultado))
  expect_true("b" %in% names(resultado))
})

test_that("left_join_all valida columnas de cruce", {
  base <- data.frame(id = 1:2)
  extra <- data.frame(codigo = 1:2)
  expect_error(
    left_join_all(base, list(extra), by = "id"),
    "Columnas no encontradas en `y_list\\[\\[1\\]\\]` para `by`: id."
  )
})

test_that("RevisarDuplicados detecta llaves repetidas", {
  a <- data.frame(id = c(1, 1, 2))
  b <- data.frame(id = c(1, 2, 2))
  resultado <- suppressMessages(RevisarDuplicados(a, b, by = "id"))
  expect_equal(nrow(resultado$duplicados_x), 2)
  expect_equal(nrow(resultado$duplicados_y), 2)
})

test_that("RevisarDuplicados valida existencia de columnas", {
  a <- data.frame(id = c(1, 1, 2))
  b <- data.frame(codigo = c(1, 2, 2))
  expect_error(
    RevisarDuplicados(a, b, by = "id"),
    "Columnas no encontradas en `y` para `by`: id."
  )
})

test_that("operador %||% funciona correctamente", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("valor" %||% "default", "valor")
  expect_equal(character(0) %||% "default", "default")
})

test_that("RecodificarTop estrategia absoluta conserva top N", {
  df <- data.frame(
    cat = c("A","A","B","B","C","D","E"),
    val = c(10, 10, 8, 8, 5, 3, 1)
  )
  resultado <- RecodificarTop(df, cat, val, "sum", "absoluto",
    n = 2, nom_var = "cat_top")
  cats_top <- unique(resultado$cat_top[resultado$cat_top != "OTROS"])
  expect_lte(length(cats_top), 2)
})

test_that("RecodificarTop valida rango y tipos de entrada", {
  df <- data.frame(cat = c("A", "B"), val = c(1, 2))
  expect_error(
    RecodificarTop(df, cat, val, "sum", "absoluto", n = 0, nom_var = "cat_top"),
    "`n` debe ser un entero mayor o igual a 1."
  )
  expect_error(
    RecodificarTop(df, cat, val, "sum", "relativo", pct_min = 2, nom_var = "cat_top"),
    "`pct_min` debe ser numerico en el rango \\[0, 1\\]."
  )
})


# ---- Formatos ----

test_that("ObtenerFormato falla con nombre invalido", {
  expect_error(ObtenerFormato("formato_que_no_existe"))
})

test_that("DefinirFormato registra formato personalizado", {
  DefinirFormato("test_fmt", scales::number_format())
  fn <- ObtenerFormato("test_fmt")
  expect_true(is.function(fn))
})

test_that("FormatoD3 retorna cadenas validas", {
  expect_equal(FormatoD3("numero"), ",.0f")
  expect_equal(FormatoD3("porcentaje"), ".1%")
  expect_equal(FormatoD3("variacion"), "+.1%")
})


# ---- Imputacion de series ----

test_that("aplicar_imputacion metodo promedio elimina NA", {
  serie <- c(1, 2, NA, 4, NA, 6)
  resultado <- aplicar_imputacion(serie, "promedio")
  expect_false(any(is.na(resultado)))
  expect_equal(length(resultado), length(serie))
})

test_that("aplicar_imputacion constante requiere valor", {
  serie <- c(1, NA, 3)
  expect_error(aplicar_imputacion(serie, "constante"))
  resultado <- aplicar_imputacion(serie, "constante", valor_constante = 0)
  expect_equal(resultado[2], 0)
})

test_that("aplicar_imputacion sin NA retorna original", {
  serie <- c(1, 2, 3, 4, 5)
  expect_equal(aplicar_imputacion(serie, "promedio"), serie)
})
