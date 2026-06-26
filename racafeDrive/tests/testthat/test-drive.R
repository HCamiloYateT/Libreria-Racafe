library(testthat)
library(racafeDrive)

# ============================================================
# Tests — racafeDrive: Microsoft Graph / OneDrive
# ============================================================

test_that("ObtenerTokenAcceso falla sin variables de entorno", {
  withr::with_envvar(
    list(
      MS_TENANT_ID     = "",
      MS_CLIENT_ID     = "",
      MS_CLIENT_SECRET = ""
    ),
    expect_error(ObtenerTokenAcceso(), "MS_TENANT_ID")
  )
})

test_that("Cache de token: segunda llamada no hace request HTTP", {
  skip_if(
    nchar(Sys.getenv("MS_TENANT_ID")) == 0,
    "Variables de entorno Microsoft Graph no configuradas"
  )
  t1 <- ObtenerTokenAcceso()
  t2 <- ObtenerTokenAcceso()
  expect_equal(t1, t2)
})

test_that("ObtenerTokenAcceso con force=TRUE renueva el cache", {
  skip_if(
    nchar(Sys.getenv("MS_TENANT_ID")) == 0,
    "Variables de entorno Microsoft Graph no configuradas"
  )
  t1 <- ObtenerTokenAcceso()
  t2 <- ObtenerTokenAcceso(force = TRUE)
  # Mismo token si no expiro, pero el request se ejecuto de nuevo
  expect_true(nchar(t2) > 0)
})

test_that("CabecerasGraph retorna lista con Authorization", {
  skip_if(
    nchar(Sys.getenv("MS_TENANT_ID")) == 0,
    "Variables de entorno Microsoft Graph no configuradas"
  )
  headers <- CabecerasGraph()
  expect_true("Authorization" %in% names(headers))
  expect_match(headers$Authorization, "^Bearer ")
})

test_that(".graph_items_a_df retorna data.frame vacio para lista vacia", {
  resultado <- racafeDrive:::.graph_items_a_df(list())
  expect_true(is.data.frame(resultado))
  expect_equal(nrow(resultado), 0)
  expect_true("nombre" %in% names(resultado))
  expect_true("id" %in% names(resultado))
})

test_that(".graph_items_a_df convierte lista de items correctamente", {
  items <- list(
    list(name = "archivo1.xlsx", id = "abc123", size = 1024L),
    list(name = "carpeta1",      id = "def456", folder = list())
  )
  resultado <- racafeDrive:::.graph_items_a_df(items)
  expect_equal(nrow(resultado), 2)
  expect_false(resultado$es_carpeta[1])
  expect_true(resultado$es_carpeta[2])
  expect_equal(resultado$nombre[1], "archivo1.xlsx")
})
