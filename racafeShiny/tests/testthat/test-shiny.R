library(testthat)
library(racafeShiny)

# ============================================================
# Tests — racafeShiny: formatos, estilos gt y componentes Shiny
# ============================================================

# ---- Registro de formatos ----

test_that("ObtenerFormato falla con nombre invalido", {
  expect_error(ObtenerFormato("formato_inexistente_xyz"))
})

test_that("DefinirFormato registra y ObtenerFormato recupera", {
  DefinirFormato("test_miles", scales::number_format(big.mark = ","))
  fn <- ObtenerFormato("test_miles")
  expect_true(is.function(fn))
  expect_true("test_miles" %in% ListarFormatos())
})

test_that("Formatos por defecto estan disponibles al cargar", {
  defaults <- c("numero","decimal","dinero","porcentaje","variacion")
  disponibles <- ListarFormatos()
  for (f in defaults) {
    expect_true(f %in% disponibles, info = sprintf("Falta: %s", f))
  }
})

test_that("FormatoD3 retorna cadena no vacia para formatos conocidos", {
  for (f in c("numero","decimal","dinero","porcentaje","variacion")) {
    expect_true(nchar(FormatoD3(f)) > 0)
  }
})

test_that("FormatoJS contiene d3.format()", {
  expect_match(FormatoJS("porcentaje"), "d3\\.format")
})

test_that("FormatoHOT retorna patron Handsontable correcto", {
  expect_equal(FormatoHOT("numero"),     "0,0")
  expect_equal(FormatoHOT("porcentaje"), "0.0%")
  expect_equal(FormatoHOT("dinero"),     "$0,0")
  expect_equal(FormatoHOT("variacion"),  "+0.0%")
})

# ---- col_kpi y chr_kpi ----

test_that("col_kpi asigna colores correctos por rango", {
  expect_equal(col_kpi(NA_real_), "#CCCCCC")
  expect_equal(col_kpi(0.70),     "#C0392B")
  expect_equal(col_kpi(0.92),     "#F39C12")
  expect_equal(col_kpi(1.05),     "#1A7A5E")
})

test_that("chr_kpi retorna unicode correcto", {
  expect_equal(chr_kpi(NA_real_), "\u2014")
  expect_equal(chr_kpi(0.70),     "\u2716")
  expect_equal(chr_kpi(0.95),     "\u26A0")
  expect_equal(chr_kpi(1.10),     "\u2714")
})

# ---- FormatearNumero y FormatearTexto ----

test_that("FormatearNumero retorna HTML", {
  resultado <- as.character(FormatearNumero(1000, "numero"))
  expect_match(resultado, "<span")
  expect_match(resultado, "1")
})

test_that("FormatearNumero aplica color verde cuando cumple meta", {
  html <- as.character(FormatearNumero(0.90, "porcentaje", meta = 0.80))
  expect_match(html, "#1A7A5E")
})

test_that("FormatearNumero aplica color rojo cuando no cumple meta", {
  html <- as.character(FormatearNumero(0.70, "porcentaje", meta = 0.90))
  expect_match(html, "#C0392B")
})

test_that("FormatearTexto retorna HTML con estilo", {
  html <- as.character(FormatearTexto("Hola", color = "#28B78D"))
  expect_match(html, "<span")
  expect_match(html, "#28B78D")
  expect_match(html, "Hola")
})

# ---- gt_mensaje_vacio ----

test_that("gt_mensaje_vacio retorna objeto gt", {
  resultado <- gt_mensaje_vacio()
  expect_true(inherits(resultado, "gt_tbl"))
})

test_that("gt_mensaje_vacio acepta mensaje personalizado", {
  resultado <- gt_mensaje_vacio("Sin datos disponibles")
  expect_true(inherits(resultado, "gt_tbl"))
})

# ---- HTML utils ----

test_that("Saltos genera n etiquetas br", {
  expect_equal(stringr::str_count(as.character(Saltos(3)), "<br>"), 3)
})

test_that("Espacios genera n entidades nbsp", {
  expect_equal(stringr::str_count(as.character(Espacios(4)), "&nbsp;"), 4)
})

test_that("Obligatorio incluye asterisco con color rojo", {
  html <- as.character(Obligatorio("Campo"))
  expect_match(html, "\\*")
  expect_match(html, "C0392B")
})

# ---- Componentes Shiny (estructura HTML) ----

test_that("BotonDescarga rechaza color invalido", {
  expect_error(BotonDescarga("id", color = ""))
  expect_error(BotonDescarga("id", color = "color_que_no_existe_xyz"))
})

test_that("BotonDescarga acepta colores por nombre R", {
  expect_no_error(BotonDescarga("id", color = "steelblue"))
  expect_no_error(BotonDescarga("id", color = "#28b78d"))
})

test_that("BotonDescarga valida size correcto", {
  expect_no_error(BotonDescarga("id", size = "sm"))
  expect_error(BotonDescarga("id", size = "xxl"))
})

test_that("Boton valida alineacion correcta", {
  expect_no_error(Boton("id", align = "right"))
  expect_no_error(Boton("id", align = "center"))
  expect_error(Boton("id", align = "middle"))
})


test_that("Boton valida size correcto", {
  expect_no_error(Boton("id", size = "sm"))
  expect_no_error(Boton("id", size = "lg"))
  expect_no_error(Boton("id", size = "xxs"))
  expect_no_error(Boton("id", size = "xxl"))
  expect_error(Boton("id", size = "xxxl"))
})

test_that("Boton incorpora clase segun size", {
  btn <- Boton("id_btn", size = "md")
  html <- as.character(btn)
  expect_match(html, "racafe-btn-guardar--md")
})

test_that("Boton permite hover_color y posicion del label", {
  btn <- Boton("id_btn2", label = "Guardar", icono = "floppy-disk",
               hover_color = "firebrick", label_posicion = "below")
  html <- as.character(btn)
  expect_match(html, "data-racafe-hover-color=\"rgb\\(178,34,34\\)\"")
  expect_match(html, "data-racafe-label-pos=\"below\"")
  expect_match(html, "racafe-btn-content--column")
  expect_no_match(html, "<style")
  expect_error(Boton("id_btn3", hover_color = ""))
  expect_error(Boton("id_btn4", hover_color = "color_no_valido"))
})
test_that("Boton admite texto, icono o ambos", {
  expect_no_error(Boton("btn_texto", label = "Guardar", icono = NULL))
  expect_no_error(Boton("btn_icono", label = NULL, icono = "floppy-disk"))
  expect_no_error(Boton("btn_ambos", label = "Guardar", icono = "floppy-disk"))
  expect_error(Boton("btn_vacio", label = NULL, icono = NULL))
})

test_that("CajaValor retorna objeto shiny.tag", {
  resultado <- CajaValor(
    valor         = 1500000,
    formato       = "dinero",
    texto         = "Ventas",
    icono         = "chart-line",
    inputId       = "btn_test",
    mostrar_boton = FALSE
  )
  expect_true(inherits(resultado, "shiny.tag"))
})
