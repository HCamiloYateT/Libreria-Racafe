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
  defaults <- c("numero","decimal","dinero","porcentaje","variacion","miles0")
  disponibles <- ListarFormatos()
  for (f in defaults) {
    expect_true(f %in% disponibles, info = sprintf("Falta: %s", f))
  }
})

test_that("FormatoD3 retorna cadena no vacia para formatos conocidos", {
  for (f in c("numero","decimal","dinero","porcentaje","variacion","miles0")) {
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


test_that("miles0 formatea miles sin decimales", {
  fn <- DefinirFormato("miles0")
  expect_equal(fn(12345), "$12")
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

test_that("BotonDescarga rechaza colores invalidos", {
  expect_error(BotonDescarga("id", color_fondo = ""))
  expect_error(BotonDescarga("id", color_fuente = ""))
  expect_error(BotonDescarga("id", color_fondo = "color_que_no_existe_xyz"))
  expect_error(BotonDescarga("id", color_fuente = "color_que_no_existe_xyz"))
  expect_error(BotonDescarga("id", color_hover = "color_que_no_existe_xyz"))
})

test_that("BotonDescarga aplica CSS vars y clases", {
  btn <- BotonDescarga(
    "descarga_1",
    icono = "file-excel",
    size = "md",
    color_fondo = "#112233",
    color_fuente = "#f5f5f5",
    color_hover = "#102030",
    title = "Descargar archivo"
  )
  html <- as.character(btn)
  expect_match(html, "racafe-btn-descarga--md")
  expect_match(html, "racafe-btn")
  expect_match(html, "--racafe-color-fondo:rgb\\(17,34,51\\)")
  expect_match(html, "--racafe-color-fuente:rgb\\(245,245,245\\)")
  expect_match(html, "--racafe-color-hover:rgb\\(16,32,48\\)")
  expect_match(html, "title=\\\"Descargar archivo\\\"")
  expect_no_match(html, "btn-default")
})

test_that("BotonDescarga valida size correcto", {
  expect_no_error(BotonDescarga("id", size = "sm"))
  expect_no_error(BotonDescarga("id", size = "xxl"))
  expect_error(BotonDescarga("id", size = "xxxl"))
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

test_that("Boton aplica CSS vars y posicion del label", {
  btn <- Boton("id_btn2", label = "Guardar", icono = "floppy-disk",
               color_fondo = "#112233", color_fuente = "#f5f5f5",
               color_hover = "firebrick", label_posicion = "below")
  html <- as.character(btn)
  expect_match(html, "--racafe-color-fondo:rgb\\(17,34,51\\)")
  expect_match(html, "--racafe-color-fuente:rgb\\(245,245,245\\)")
  expect_match(html, "--racafe-color-hover:rgb\\(178,34,34\\)")
  expect_match(html, "data-racafe-label-pos=\"below\"")
  expect_match(html, "racafe-btn-content--column")
  expect_no_match(html, "<style")
  expect_error(Boton("id_btn3", color_hover = ""))
  expect_error(Boton("id_btn4", color_hover = "color_no_valido"))
})

test_that("Boton permite titulo hover", {
  btn <- Boton("id_btn5", titulo = "Guardar cambios")
  html <- as.character(btn)
  expect_match(html, "title=\"Guardar cambios\"")
  expect_error(Boton("id_btn6", titulo = c("A", "B")))
})
test_that("Boton admite texto, icono o ambos", {
  expect_no_error(Boton("btn_texto", label = "Guardar", icono = NULL))
  expect_no_error(Boton("btn_icono", label = NULL, icono = "floppy-disk"))
  expect_no_error(Boton("btn_ambos", label = "Guardar", icono = "floppy-disk"))
  expect_error(Boton("btn_vacio", label = NULL, icono = NULL))
})

test_that("BotonesRadiales inyecta variables CSS parametrizadas", {
  grp <- BotonesRadiales(
    inputId = "estado",
    choices = c("Activo", "Inactivo"),
    color_inactivo = "#112233",
    color_activo = "#445566"
  )
  html <- as.character(grp)
  expect_match(html, "racafe-radio-group")
  expect_match(html, "--racafe-radio-inactivo:rgb\\(17,34,51\\)")
  expect_match(html, "--racafe-radio-fuente-inactivo:rgb\\(255,255,255\\)")
  expect_match(html, "--racafe-radio-hover-inactivo:rgb\\(13,27,41\\)")
  expect_match(html, "--racafe-radio-activo:rgb\\(68,85,102\\)")
  expect_match(html, "--racafe-radio-fuente-activo:rgb\\(255,255,255\\)")
  expect_match(html, "--racafe-radio-hover-activo:rgb\\(55,69,83\\)")
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

test_that("InputFecha crea widget y deriva parametros por tipo", {
  dia <- InputFecha("f_dia", tipo = "dia", value = as.Date("2026-04-23"))
  mes <- InputFecha("f_mes", tipo = "mes", value = as.Date("2026-04-23"))
  anio <- InputFecha("f_anio", tipo = "anio", value = as.Date("2026-04-23"))

  dia_html <- as.character(dia)
  mes_html <- as.character(mes)
  anio_html <- as.character(anio)

  expect_match(dia_html, "yyyy-MM-dd")
  expect_match(mes_html, "yyyy-MM")
  expect_match(anio_html, "yyyy")
  expect_match(mes_html, "monthsShort")
  expect_match(anio_html, "language=\\\"es\\\"|language:\\\"es\\\"|\\\"language\\\":\\\"es\\\"")
})

test_that("InputFecha valida y ajusta fechas", {
  expect_error(InputFecha("f_bad", value = "no_fecha"), "fecha valida")
  expect_error(InputFecha("f_bad_min", min_date = "x"), "fecha valida")
  expect_error(InputFecha("f_bad_max", max_date = "x"), "fecha valida")

  clamped <- InputFecha(
    id = "f_clamp",
    value = as.Date("2026-01-01"),
    min_date = as.Date("2026-03-01"),
    max_date = as.Date("2026-12-31")
  )
  expect_match(as.character(clamped), "2026-03-01")
})


test_that("InputMes configura selector mensual", {
  mes <- InputMes("f_mes_wrapper", value = as.Date("2026-04-23"))
  mes_html <- as.character(mes)

  expect_match(mes_html, "yyyy-MM")
  expect_match(mes_html, "monthsShort")
})
