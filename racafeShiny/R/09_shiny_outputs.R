# ============================================================
# Seccion 09 — Componentes Shiny: Outputs y Modulos
# Botones de descarga, cajas de indicadores, modulo CajaValor
# ============================================================


# ---- Botones de descarga ----

#' Botón de descarga con el estilo visual de racafe
#'
#' Genera un `downloadButton` visualmente idéntico a [Boton()] usando su
#' propia clase base `racafe-btn-descarga`, independiente de
#' `racafe-btn-guardar`. Requiere el bloque `.racafe-btn-descarga` en
#' `style.css`. Debe usarse junto con un `downloadHandler` en el server
#' bajo el mismo `id`.
#'
#' @param id String. `outputId` del botón. Debe coincidir con el
#'   `downloadHandler` registrado en el server.
#' @param label String o `NULL`. Texto visible. `NULL` muestra solo el ícono.
#' @param icono String o `NULL`. Nombre FontAwesome. Default `"download"`.
#' @param align String. Alineación del contenedor: `"left"`, `"center"`,
#'   `"right"`. Default `"right"`.
#' @param size String. Tamaño del botón: `"xxs"`, `"xs"`, `"sm"`, `"md"`,
#'   `"lg"`, `"xl"`, `"xxl"`. Default `"sm"`.
#' @param hover_color String. Color R válido aplicado al hover. Default
#'   `"firebrick"`.
#' @param label_posicion String. Posición del label respecto al ícono:
#'   `"right"` o `"below"`. Default `"right"`.
#' @param titulo String o `NULL`. Tooltip HTML (atributo `title`). Default `NULL`.
#' @param ... Argumentos adicionales pasados a `shiny::downloadButton()`.
#'
#' @return `shiny.tag` div contenedor con el `downloadButton` estilizado.
#' @export
BotonDescarga <- function(
    id,
    label          = "Descargar",
    icono          = "download",
    align          = "right",
    size           = "sm",
    hover_color    = "firebrick",
    label_posicion = "right",
    titulo         = NULL,
    ...
) {
  # Validación de argumentos ----
  align          <- match.arg(align, c("left", "center", "right"))
  size           <- match.arg(size, c("xxs", "xs", "sm", "md", "lg", "xl", "xxl"))
  label_posicion <- match.arg(label_posicion, c("right", "below"))

  if (is.null(label) && is.null(icono)) {
    stop("Debe especificar al menos `label` o `icono`.", call. = FALSE)
  }
  if (!is.character(hover_color) || nchar(hover_color) == 0) {
    stop("`hover_color` debe ser una cadena de color valida.", call. = FALSE)
  }
  if (!is.null(titulo) && (!is.character(titulo) || length(titulo) != 1)) {
    stop("`titulo` debe ser `NULL` o una cadena de longitud 1.", call. = FALSE)
  }

  # Resolución del color hover a RGB ----
  hover_color_css <- tryCatch({
    rgb_vals <- grDevices::col2rgb(hover_color)
    sprintf("rgb(%d,%d,%d)", rgb_vals[1], rgb_vals[2], rgb_vals[3])
  }, error = function(e) {
    stop(sprintf("Color '%s' no reconocido.", hover_color), call. = FALSE)
  })

  # Clases y contenido interno ----
  clase_align <- switch(align, left = "text-left", center = "text-center", right = "text-right")

  clase_label <- if (!is.null(icono) && !is.null(label) && identical(label_posicion, "below")) {
    "racafe-btn-content racafe-btn-content--column"
  } else {
    "racafe-btn-content"
  }

  contenido_boton <- shiny::tags$span(
    class = clase_label,
    if (!is.null(icono)) shiny::icon(icono, class = "racafe-btn-icon") else NULL,
    if (!is.null(label)) shiny::tags$span(class = "racafe-btn-label", label) else NULL
  )

  clase_boton <- c(
    "btn btn-success racafe-btn-descarga",
    sprintf("racafe-btn-descarga--%s", size),
    "racafe-btn-content-host",
    if (identical(label_posicion, "below")) "racafe-btn-content-host--column" else NULL
  )

  boton <- shiny::downloadButton(
    outputId = id,
    label    = contenido_boton,
    class    = paste(clase_boton, collapse = " "),
    icon     = NULL,
    ...
  )

  boton <- shiny::tagAppendAttributes(
    boton,
    `data-racafe-label-pos` = label_posicion,
    title                   = titulo
  )

  # Estilo hover escopado por ID ----
  # Se inyecta un <style> por instancia porque el JS de hover_color opera
  # sobre <button> y no alcanza el <a> que genera downloadButton. El selector
  # #id.racafe-btn-descarga garantiza que no afecta ningun otro elemento.
  hover_style <- shiny::tags$style(sprintf(
    "#%s.racafe-btn-descarga:hover,\n     #%s.racafe-btn-descarga:focus,\n     #%s.racafe-btn-descarga:active {\n       background-color: %s !important;\n       border-color:     %s !important;\n     }",
    id, id, id, hover_color_css, hover_color_css
  ))

  shiny::tagList(
    hover_style,
    shiny::div(class = clase_align, boton)
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
