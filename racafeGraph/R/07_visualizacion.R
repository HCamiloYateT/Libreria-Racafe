# ============================================================
# Seccion 07 — Visualizacion
# Paletas, lineas de referencia y graficos corporativos plotly
# ============================================================


# ---- Paletas de colores ----

#' Generar paleta de colores corporativos Racafe
#'
#' @param n Numero de colores requeridos. Maximo 10.
#' @return Vector de colores hexadecimales.
#' @export
#' @examples
#' ColoresRacafe(5)
#' ColoresRacafe(1)
ColoresRacafe <- function(n = 10) {
  n <- as.integer(n)
  if (n < 1) stop("n debe ser mayor o igual a 1.", call. = FALSE)
  if (n > length(racafeCore::colores_corporativos())) {
    # Interpolar si se piden mas colores que los disponibles
    colorRampPalette(racafeCore::colores_corporativos())(n)
  } else {
    racafeCore::colores_corporativos()[seq_len(n)]
  }
}


#' Paleta gradiente verde-azul segun valores numericos
#'
#' @param value Vector numerico a mapear a colores.
#' @return Vector de colores hexadecimales de la misma longitud que `value`.
#' @export
#' @examples
#' ColoresGreenBlue(seq(0, 1, length.out = 5))
#' ColoresGreenBlue(1:10)
ColoresGreenBlue <- function(value) {
  paleta <- colorRampPalette(c("#28B78D", "#1A5276"))(100)
  rango  <- range(value, na.rm = TRUE)

  if (rango[1] == rango[2]) {
    return(rep(paleta[50], length(value)))
  }

  indices <- round(
    (value - rango[1]) / (rango[2] - rango[1]) * 99
  ) + 1
  indices[is.na(indices)] <- 1
  paleta[indices]
}


# ---- Tema corporativo plotly ----

#' Obtener lista de configuracion de layout corporativo para plotly
#'
#' Aplicar con `plotly::layout(p, !!!tema_racafe_plotly())`.
#'
#' @param titulo Titulo principal del grafico. `NULL` omite titulo.
#' @param subtitulo Subtitulo. `NULL` omite.
#' @return Lista con parametros de layout para plotly.
#' @export
tema_racafe_plotly <- function(titulo = NULL, subtitulo = NULL) {
  lista <- list(
    font = list(
      family = "Roboto, Arial, sans-serif",
      size   = 12,
      color  = "#333333"
    ),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    xaxis = list(
      gridcolor   = "#E8E8E8",
      linecolor   = "#CCCCCC",
      zerolinecolor = "#CCCCCC"
    ),
    yaxis = list(
      gridcolor   = "#E8E8E8",
      linecolor   = "#CCCCCC",
      zerolinecolor = "#CCCCCC"
    ),
    legend = list(
      bgcolor      = "rgba(255,255,255,0.8)",
      bordercolor  = "#DDDDDD",
      borderwidth  = 1
    ),
    margin = list(l = 50, r = 30, t = 50, b = 50)
  )

  if (!is.null(titulo)) {
    lista$title <- list(
      text = titulo,
      font = list(size = 15, color = "#1A1A1A"),
      x    = 0.02
    )
  }

  lista
}


# ---- Lineas de referencia ----

#' Crear linea vertical de referencia para graficos plotly
#'
#' @param x Posicion en el eje X.
#' @param color Color de la linea.
#' @return Lista con especificacion de shape para `plotly::layout(shapes = ...)`.
#' @export
#' @examples
#' \dontrun{
#'   plotly::plot_ly() |>
#'     plotly::layout(shapes = list(vline(10, "#28B78D")))
#' }
vline <- function(x = 0, color = "red") {
  list(
    type      = "line",
    x0        = x,
    x1        = x,
    yref      = "paper",
    y0        = 0,
    y1        = 1,
    line      = list(color = color, width = 1.5, dash = "dot")
  )
}


#' Crear linea horizontal de referencia para graficos plotly
#'
#' @param y Posicion en el eje Y.
#' @param color Color de la linea.
#' @return Lista con especificacion de shape para `plotly::layout(shapes = ...)`.
#' @export
#' @examples
#' \dontrun{
#'   plotly::plot_ly() |>
#'     plotly::layout(shapes = list(hline(0.5)))
#' }
hline <- function(y = 0, color = "#ff3a21") {
  list(
    type      = "line",
    xref      = "paper",
    x0        = 0,
    x1        = 1,
    y0        = y,
    y1        = y,
    line      = list(color = color, width = 1.5, dash = "dot")
  )
}


# ---- Graficos corporativos ----

#' Histograma con densidad kernel en escala logaritmica
#'
#' Combina histograma (porcentaje) y densidad kernel para distribuciones
#' sesgadas. Util para explorar variables con colas largas (ingresos, volumen).
#'
#' @param datos `data.frame` con los datos.
#' @param columna Nombre de la columna numerica a graficar.
#' @param titulo Titulo del grafico.
#' @param formato Argumento reservado para compatibilidad (sin uso actual).
#' @return Objeto plotly.
#' @export
#' @examples
#' set.seed(123)
#' ventas <- data.frame(ingresos = rgamma(250, shape = 3, rate = 0.7))
#' ImprimirDensidad(ventas, "ingresos", "Ingresos diarios", formato = "numero")
ImprimirDensidad <- function(datos, columna, titulo, formato = "numero") {
  x <- datos[[columna]]
  x <- x[!is.na(x) & is.finite(x)]

  if (length(x) == 0) {
    stop("La columna no contiene valores validos.", call. = FALSE)
  }

  densidad <- stats::density(x, na.rm = TRUE)

  p <- plotly::plot_ly() |>
    plotly::add_histogram(
      x          = x,
      histnorm   = "probability",
      name       = "Frecuencia",
      marker     = list(
        color = ColoresRacafe(1),
        line  = list(color = "white", width = 0.5)
      ),
      hovertemplate = paste0("<b>Rango:</b> %{x}<br>",
                             "<b>Proporcion:</b> %{y:.1%}<extra></extra>")
    ) |>
    plotly::add_lines(
      x    = densidad$x,
      y    = densidad$y,
      name = "Densidad",
      line = list(color = ColoresRacafe(2)[2], width = 2),
      yaxis = "y2",
      hovertemplate = "<b>Densidad:</b> %{y:.4f}<extra></extra>"
    ) |>
    plotly::layout(
      title   = list(text = titulo, x = 0.02, font = list(size = 14)),
      xaxis   = list(
        title = columna,
        type  = "log",
        tickformat = ""
      ),
      yaxis   = list(title = "Proporcion", tickformat = ".0%"),
      yaxis2  = list(
        title    = "Densidad",
        overlaying = "y",
        side     = "right"
      ),
      legend  = list(x = 0.75, y = 0.95),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )

  p
}


#' Grafico de anillo con plotly
#'
#' Agrega los datos segun la variable de medida y genera un donut chart.
#'
#' @param data `data.frame` con los datos.
#' @param var_label Nombre de la variable categorica (etiquetas).
#' @param var_medida Nombre de la variable numerica. `NULL` cuenta registros.
#' @param funcion Funcion de agregacion: `"sum"` o `"n"`.
#' @param colores Vector de colores. `NULL` usa la paleta corporativa.
#' @return Objeto plotly.
#' @export
#' @examples
#' df <- data.frame(
#'   categoria = c("A","A","B","C","C","C"),
#'   valor = c(10, 20, 30, 5, 15, 25)
#' )
#' ImprimirAnillo(df, var_label = "categoria", var_medida = "valor")
ImprimirAnillo <- function(
    data,
    var_label,
    var_medida = NULL,
    funcion    = c("sum", "n"),
    colores    = NULL) {

  funcion <- match.arg(funcion)

  # Agregacion de datos (separado del render)
  datos_agg <- .agregar_anillo(data, var_label, var_medida, funcion)

  n_cats  <- nrow(datos_agg)
  colores <- colores %||% ColoresRacafe(n_cats)

  plotly::plot_ly(
    data   = datos_agg,
    labels = ~etiqueta,
    values = ~valor,
    type   = "pie",
    hole   = 0.55,
    marker = list(colors = colores, line = list(color = "white", width = 2)),
    textinfo   = "label+percent",
    hovertemplate = paste0(
      "<b>%{label}</b><br>",
      "Valor: %{value:,.0f}<br>",
      "Participacion: %{percent}<extra></extra>"
    )
  ) |>
    plotly::layout(
      showlegend    = TRUE,
      legend        = list(orientation = "v", x = 1.05),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin        = list(l = 20, r = 120, t = 20, b = 20)
    )
}


#' Diagrama Sankey con plotly
#'
#' @param data `data.frame` con los datos de flujo.
#' @param vars Vector de nombres de columnas que definen los nodos
#'   (origen -> destino -> ...).
#' @param fun Funcion de agregacion: `"sum"` o `"n"`.
#' @param var Nombre de la variable numerica para `fun = "sum"`. `NULL` cuenta.
#' @param colores Vector de colores para los nodos.
#' @return Objeto plotly con diagrama Sankey.
#' @export
#' @examples
#' \dontrun{
#'   df <- data.frame(origen = c("A","A","B"), destino = c("X","Y","X"),
#'                    valor = c(10, 20, 15))
#'   ImprimeSankey(df, vars = c("origen","destino"), fun = "sum", var = "valor")
#' }
ImprimeSankey <- function(data, vars, fun, var = NULL, colores) {
  # Preparacion de nodos y enlaces (logica separada del render)
  sankey_data <- .preparar_sankey(data, vars, fun, var)

  n_nodos <- length(sankey_data$nodos)
  if (missing(colores)) {
    colores <- ColoresRacafe(min(n_nodos, 10))
    if (n_nodos > 10) {
      colores <- colorRampPalette(colores)(n_nodos)
    }
  }

  plotly::plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = sankey_data$nodos,
      color = colores[seq_len(n_nodos)],
      pad   = 15,
      thickness = 20
    ),
    link = list(
      source = sankey_data$fuente,
      target = sankey_data$destino,
      value  = sankey_data$valor
    )
  ) |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      font  = list(size = 11, color = "#333333"),
      margin = list(l = 20, r = 20, t = 20, b = 20)
    )
}


# ---- Funciones internas de agregacion ----

# Agrega datos para el grafico de anillo
.agregar_anillo <- function(data, var_label, var_medida, funcion) {
  if (funcion == "n" || is.null(var_medida)) {
    data |>
      dplyr::group_by(etiqueta = .data[[var_label]]) |>
      dplyr::summarise(valor = dplyr::n(), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$valor))
  } else {
    data |>
      dplyr::group_by(etiqueta = .data[[var_label]]) |>
      dplyr::summarise(
        valor = sum(.data[[var_medida]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$valor))
  }
}

# Prepara estructura de nodos y enlaces para Sankey
.preparar_sankey <- function(data, vars, fun, var) {
  pares <- purrr::map2(
    vars[-length(vars)],
    vars[-1],
    function(origen, destino) {
      if (fun == "n" || is.null(var)) {
        data |>
          dplyr::group_by(
            fuente  = .data[[origen]],
            destino = .data[[destino]]
          ) |>
          dplyr::summarise(valor = dplyr::n(), .groups = "drop")
      } else {
        data |>
          dplyr::group_by(
            fuente  = .data[[origen]],
            destino = .data[[destino]]
          ) |>
          dplyr::summarise(
            valor = sum(.data[[var]], na.rm = TRUE),
            .groups = "drop"
          )
      }
    }
  )

  enlaces <- dplyr::bind_rows(pares)
  nodos   <- unique(c(enlaces$fuente, enlaces$destino))

  list(
    nodos   = nodos,
    fuente  = match(enlaces$fuente, nodos) - 1L,
    destino = match(enlaces$destino, nodos) - 1L,
    valor   = enlaces$valor
  )
}
