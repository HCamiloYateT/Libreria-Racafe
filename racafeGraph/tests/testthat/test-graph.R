library(testthat)
library(racafeGraph)

# ============================================================
# Tests — racafeGraph: paletas, lineas y graficos plotly
# ============================================================

test_that("ColoresRacafe retorna n colores validos", {
  for (n in c(1, 3, 5, 10)) {
    cols <- ColoresRacafe(n)
    expect_length(cols, n)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
  }
})

test_that("ColoresRacafe interpola cuando n > 10", {
  cols <- ColoresRacafe(20)
  expect_length(cols, 20)
})

test_that("ColoresRacafe rechaza n < 1", {
  expect_error(ColoresRacafe(0))
  expect_error(ColoresRacafe(-1))
})

test_that("ColoresGreenBlue mapea correctamente", {
  vals <- seq(0, 1, length.out = 5)
  cols <- ColoresGreenBlue(vals)
  expect_length(cols, 5)
  expect_true(all(grepl("^#", cols)))
})

test_that("ColoresGreenBlue maneja vector constante sin error", {
  cols <- ColoresGreenBlue(c(5, 5, 5))
  expect_length(cols, 3)
  expect_equal(length(unique(cols)), 1)
})

test_that("vline genera shape plotly valida", {
  s <- vline(50, "#28B78D")
  expect_equal(s$type, "line")
  expect_equal(s$x0, 50)
  expect_equal(s$x1, 50)
  expect_equal(s$line$color, "#28B78D")
  expect_equal(s$yref, "paper")
})

test_that("hline genera shape plotly valida", {
  s <- hline(0.75, "#FF0000")
  expect_equal(s$type, "line")
  expect_equal(s$y0, 0.75)
  expect_equal(s$y1, 0.75)
  expect_equal(s$xref, "paper")
})

test_that("tema_racafe_plotly retorna lista con claves esperadas", {
  t <- tema_racafe_plotly()
  expect_true(is.list(t))
  expect_true(all(c("font", "paper_bgcolor", "xaxis", "yaxis") %in% names(t)))
})

test_that("tema_racafe_plotly incluye titulo solo si se pasa", {
  sin_titulo <- tema_racafe_plotly()
  con_titulo <- tema_racafe_plotly("Mi titulo")
  expect_false("title" %in% names(sin_titulo))
  expect_equal(con_titulo$title$text, "Mi titulo")
})

test_that("ImprimirAnillo retorna objeto plotly", {
  df <- data.frame(
    cat = c("A","A","B","C","C","C"),
    val = c(10, 20, 30, 5, 15, 25)
  )
  p <- ImprimirAnillo(df, var_label = "cat", var_medida = "val")
  expect_true(inherits(p, "plotly"))
})

test_that("ImprimirAnillo funciona con funcion = n", {
  df <- data.frame(cat = c("A","A","B","B","B"))
  p <- ImprimirAnillo(df, var_label = "cat", funcion = "n")
  expect_true(inherits(p, "plotly"))
})

test_that("ImprimirDensidad retorna objeto plotly", {
  set.seed(7)
  df <- data.frame(x = rgamma(80, shape = 2, rate = 0.5))
  p <- ImprimirDensidad(df, "x", "Test", formato = "numero")
  expect_true(inherits(p, "plotly"))
})

test_that("ImprimirDensidad falla con columna toda NA", {
  df <- data.frame(x = rep(NA_real_, 10))
  expect_error(ImprimirDensidad(df, "x", "Test"))
})

test_that("ImprimeSankey retorna objeto plotly", {
  df <- data.frame(
    origen  = c("A","A","B","B"),
    destino = c("X","Y","X","Z"),
    valor   = c(10, 20, 15, 5)
  )
  p <- ImprimeSankey(
    df, vars = c("origen","destino"),
    fun = "sum", var = "valor",
    colores = ColoresRacafe(4)
  )
  expect_true(inherits(p, "plotly"))
})

test_that(".preparar_sankey produce indices base-0 para plotly", {
  df <- data.frame(
    a = c("X","X","Y"), b = c("M","N","M"), v = c(1,2,3)
  )
  res <- racafeGraph:::.preparar_sankey(df, c("a","b"), "sum", "v")
  expect_true(all(res$fuente  >= 0))
  expect_true(all(res$destino >= 0))
  expect_equal(length(res$fuente), length(res$valor))
})

test_that(".agregar_anillo suma por categoria", {
  df <- data.frame(cat = c("A","A","B"), val = c(10,20,30))
  res <- racafeGraph:::.agregar_anillo(df, "cat", "val", "sum")
  expect_equal(res$valor[res$etiqueta == "A"], 30)
  expect_equal(res$valor[res$etiqueta == "B"], 30)
})
