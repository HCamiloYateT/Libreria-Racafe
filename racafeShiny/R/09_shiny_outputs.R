# ============================================================
# Seccion 09 — Componentes Shiny: Outputs y Modulos
# Botones de descarga, cajas de indicadores, modulo CajaValor
# ============================================================


# ---- Botones de descarga ----

#' Boton de descarga personalizado con icono y color configurable
#'
#' @param button_id ID del boton de descarga.
#' @param icon_name Nombre del icono FontAwesome (sin prefijo `fa-`).
#' @param color Color del boton. Cualquier color valido en R o hexadecimal.
#' @param ns Funcion namespace de modulo Shiny. `NULL` si no es modulo.
#' @param size Tamano del boton: `"xs"`, `"sm"`, `"md"`, `"lg"`.
#' @param title Tooltip del boton.
#' @param ... Argumentos adicionales para `shiny::downloadButton`.
#' @return Componente Shiny con boton de descarga.
#' @export
#' @examples
#' \dontrun{
#'   BotonDescarga("descargar")
#'   BotonDescarga("descargar_resumen", color = "steelblue", size = "md",
#'     title = "Descargar resumen semanal")
#' }
BotonDescarga <- function(
    button_id,
    icon_name = "file-excel",
    color     = "#28b78d",
    ns        = NULL,
    size      = "sm",
    title     = "Descargar",
    ...) {

  # Validacion de argumentos
  size <- match.arg(size, c("xs", "sm", "md", "lg"))
  if (!is.character(color) || nchar(color) == 0) {
    stop("`color` debe ser una cadena de color valida.", call. = FALSE)
  }

  # Normalizar color a hex
  color_hex <- tryCatch(
    {
      rgb_vals <- grDevices::col2rgb(color)
      sprintf(
        "rgb(%d,%d,%d)",
        rgb_vals[1], rgb_vals[2], rgb_vals[3]
      )
    },
    error = function(e) {
      stop(sprintf("Color '%s' no reconocido.", color), call. = FALSE)
    }
  )

  input_id <- if (!is.null(ns)) ns(button_id) else button_id

  shiny::downloadButton(
    outputId = input_id,
    label    = shiny::tagList(
      shiny::icon(icon_name),
      shiny::tags$span(title)
    ),
    class    = sprintf("btn btn-%s racafe-btn-descarga", size),
    style    = sprintf(
      "background-color:%s;border-color:%s;color:white;",
      color_hex, color_hex
    ),
    ...
  )
}


# ---- Cajas de indicadores ----

#' Caja informativa con icono y color personalizable
#'
#' @param texto Texto principal a mostrar.
#' @param icono Nombre del icono FontAwesome.
#' @param col_fondo Color de fondo de la caja.
#' @param alto Alto de la caja en pixeles.
#' @param col_letra Color del texto.
#' @param col_icono Color del icono.
#' @return Objeto HTML de Shiny.
#' @export
#' @examples
#' \dontrun{
#'   CajaIco("Ingresos totales", "chart-line")
#' }
CajaIco <- function(
    texto,
    icono,
    col_fondo  = "#FDFEFE",
    alto       = 120,
    col_letra  = "#17202A",
    col_icono  = "#28B78D") {

  shiny::div(
    class = "racafe-caja-ico",
    style = sprintf(
      "background-color:%s;height:%spx;display:flex;align-items:center;
       padding:16px;border-radius:8px;box-shadow:0 1px 4px rgba(0,0,0,0.08);",
      col_fondo, alto
    ),
    shiny::div(
      style = sprintf(
        "font-size:2.5em;color:%s;margin-right:16px;flex-shrink:0;",
        col_icono
      ),
      shiny::icon(icono)
    ),
    shiny::div(
      style = sprintf(
        "color:%s;font-size:0.95em;font-weight:500;line-height:1.4;",
        col_letra
      ),
      texto
    )
  )
}


#' Caja de indicador con valor formateado y boton de detalle
#'
#' @param valor Valor numerico a mostrar.
#' @param formato Nombre del formato registrado.
#' @param texto Descripcion del indicador.
#' @param icono Nombre del icono FontAwesome.
#' @param inputId ID del boton de detalle.
#' @param mostrar_boton Logico. Mostrar boton de detalle.
#' @return Objeto HTML de Shiny.
#' @export
#' @examples
#' \dontrun{
#'   CajaValor(1250000, "dinero", "Ingresos mensuales", "chart-line",
#'     "detalle_ingresos")
#' }
CajaValor <- function(
    valor,
    formato,
    texto,
    icono,
    inputId,
    mostrar_boton = TRUE) {

  valor_html <- FormatearNumero(valor, formato, color = "#1A1A1A")

  shiny::div(
    class = "racafe-caja-valor",
    style = paste(
      "background:white;border-radius:8px;padding:16px;",
      "box-shadow:0 1px 4px rgba(0,0,0,0.08);",
      "display:flex;flex-direction:column;gap:8px;"
    ),
    shiny::div(
      style = "display:flex;align-items:center;justify-content:space-between;",
      shiny::div(
        style = "display:flex;align-items:center;gap:10px;",
        shiny::div(
          style = "color:#28B78D;font-size:1.6em;",
          shiny::icon(icono)
        ),
        shiny::div(
          style = "font-size:0.85em;color:#666;font-weight:500;",
          texto
        )
      ),
      if (mostrar_boton) {
        shiny::actionButton(
          inputId = inputId,
          label   = NULL,
          icon    = shiny::icon("magnifying-glass"),
          class   = "btn btn-xs btn-outline-secondary",
          style   = "padding:2px 6px;border-radius:4px;"
        )
      }
    ),
    shiny::div(
      style = "font-size:1.5em;font-weight:700;",
      valor_html
    )
  )
}


# ---- Modulo CajaValor (patron UI / Server separados) ----

#' UI del modulo CajaValor
#'
#' Genera la estructura HTML del indicador. Toda la logica numerica
#' vive en el servidor del modulo (`cajaValor_server`).
#'
#' @param id ID del modulo Shiny.
#' @param icono Nombre del icono FontAwesome.
#' @param texto Etiqueta descriptiva del indicador.
#' @param mostrar_boton Logico. Mostrar boton de detalle.
#' @return `tagList` con la UI del modulo.
#' @export
cajaValor_ui <- function(id, icono = "chart-line", texto = "",
                         mostrar_boton = TRUE) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "racafe-caja-valor",
    style = paste(
      "background:white;border-radius:8px;padding:16px;",
      "box-shadow:0 1px 4px rgba(0,0,0,0.08);",
      "display:flex;flex-direction:column;gap:8px;"
    ),
    shiny::div(
      style = "display:flex;align-items:center;justify-content:space-between;",
      shiny::div(
        style = "display:flex;align-items:center;gap:10px;",
        shiny::div(
          style = "color:#28B78D;font-size:1.6em;",
          shiny::icon(icono)
        ),
        shiny::div(
          style = "font-size:0.85em;color:#666;font-weight:500;",
          texto
        )
      ),
      if (mostrar_boton) {
        shiny::actionButton(
          inputId = ns("detalle"),
          label   = NULL,
          icon    = shiny::icon("magnifying-glass"),
          class   = "btn btn-xs btn-outline-secondary",
          style   = "padding:2px 6px;border-radius:4px;"
        )
      }
    ),
    shiny::div(
      style = "font-size:1.5em;font-weight:700;",
      shiny::uiOutput(ns("valor_display"))
    )
  )
}


#' Server del modulo CajaValor
#'
#' Recibe un reactivo con el valor y aplica formato. Exporta el
#' evento del boton de detalle para el modulo padre.
#'
#' @param id ID del modulo Shiny.
#' @param valor_r Reactivo que retorna el valor numerico.
#' @param formato Nombre del formato registrado en `DefinirFormato`.
#' @param meta Valor de meta para colorear condicionalmente. `NA` desactiva.
#' @return Lista con `click_detalle`: reactivo del boton de detalle.
#' @export
#' @examples
#' \dontrun{
#'   # En UI del modulo padre:
#'   cajaValor_ui("ingresos", icono = "dollar-sign", texto = "Ingresos")
#'
#'   # En Server del modulo padre:
#'   modulo <- cajaValor_server("ingresos",
#'     valor_r = reactive(sum(datos()$ventas)),
#'     formato = "dinero"
#'   )
#'   observeEvent(modulo$click_detalle(), { ... })
#' }
cajaValor_server <- function(id, valor_r, formato, meta = NA) {
  shiny::moduleServer(id, function(input, output, session) {

    output$valor_display <- shiny::renderUI({
      shiny::req(valor_r())
      FormatearNumero(valor_r(), formato, meta = meta)
    })

    list(
      click_detalle = shiny::reactive(input$detalle)
    )
  })
}
