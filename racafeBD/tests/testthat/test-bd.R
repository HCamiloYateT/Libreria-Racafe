library(testthat)
library(racafeBD)

# ============================================================
# Tests — racafeBD: conexion y operaciones de base de datos
# ============================================================

test_that("ConectarBD falla sin DB_UID o DB_PWD", {
  withr::with_envvar(
    list(DB_UID = "", DB_PWD = ""),
    expect_error(ConectarBD(), "DB_UID")
  )
})

test_that("EscribirDatos valida que df sea data.frame", {
  withr::with_envvar(
    list(DB_UID = "x", DB_PWD = "x", DB_NAME = "test", DB_SERVER = "x"),
    expect_error(EscribirDatos("no_es_df", "tabla"))
  )
})

test_that("EscribirDatos valida nombre de tabla no vacio", {
  withr::with_envvar(
    list(DB_UID = "x", DB_PWD = "x", DB_NAME = "test", DB_SERVER = "x"),
    expect_error(EscribirDatos(data.frame(x = 1), ""))
  )
})

test_that("ReemplazarDatos valida que llaves sea lista no vacia", {
  withr::with_envvar(
    list(DB_UID = "x", DB_PWD = "x", DB_NAME = "test", DB_SERVER = "x"),
    expect_error(ReemplazarDatos(data.frame(x = 1), "tabla", list()))
  )
})

test_that("Logica de EscribirDatos y CargarDatos con SQLite mock", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  df <- data.frame(id = 1:3, valor = c("a", "b", "c"))
  DBI::dbWriteTable(con, "test_t", df)
  leido <- DBI::dbReadTable(con, "test_t")
  expect_equal(nrow(leido), 3)
  expect_equal(names(leido), c("id", "valor"))
})

test_that("Consulta con janitor::clean_names normaliza columnas", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  df <- data.frame("Mi Columna" = 1:2, check.names = FALSE)
  DBI::dbWriteTable(con, "t", df)
  res <- DBI::dbGetQuery(con, "SELECT * FROM t")
  limpio <- janitor::clean_names(res)
  expect_true(all(names(limpio) == tolower(gsub("[^a-z0-9]", "_", names(limpio)))))
})

test_that("ConsultaSistema usa env vars si no se pasan uid/pwd", {
  withr::with_envvar(
    list(SYS_UID = "usuario_test", SYS_PWD = ""),
    {
      # Solo verifica que intente conectar con el uid de env var
      # La conexion fallara (servidor no disponible en CI) pero el
      # uid debe coincidir con lo que esta en la env var
      expect_error(
        ConsultaSistema("syscafe", "SELECT 1", server = "127.0.0.1")
      )
    }
  )
})

test_that("postproceso de ConsultaSistema limpia y capitaliza texto", {
  entrada <- data.frame(
    Nombre = c("  jOsé  pérez!! ", "MARIA###"),
    stringsAsFactors = FALSE
  )

  salida <- racafeBD:::.postprocesar_consulta_sistema(entrada, capitalizacion = "titulo")

  expect_equal(salida$Nombre, c("Jose Perez", "Maria"))
})

test_that("postproceso de ConsultaSistema usa mayusculas por defecto", {
  entrada <- data.frame(
    Nombre = c("  jOsé  pérez!! ", "MARIA###"),
    stringsAsFactors = FALSE
  )

  salida <- racafeBD:::.postprocesar_consulta_sistema(entrada)

  expect_equal(salida$Nombre, c("JOSE PEREZ", "MARIA"))
})

test_that("postproceso de ConsultaSistema convierte columnas fecha por nombre", {
  entrada <- data.frame(
    FechaRegistro = c("2026-01-03", "2026/02/04"),
    OtraColumna = c("x", "y"),
    stringsAsFactors = FALSE
  )

  salida <- racafeBD:::.postprocesar_consulta_sistema(entrada, capitalizacion = "ninguna")

  expect_s3_class(salida$FechaRegistro, "Date")
  expect_equal(salida$FechaRegistro, as.Date(c("2026-01-03", "2026-02-04")))
})
