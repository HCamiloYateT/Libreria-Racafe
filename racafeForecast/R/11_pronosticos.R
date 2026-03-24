# ============================================================
# Seccion 11 — Pronosticos de series de tiempo
# Imputacion, modelos, metricas y seleccion del mejor modelo
# ============================================================


# ---- Clase S3 para resultados de pronostico ----

# Constructor interno para objeto de resultado estandarizado
.nuevo_resultado_pronostico <- function(
    resultados,
    columnas,
    h_periods,
    nivel_confianza,
    fecha_col) {

  estructura <- list(
    resultados      = resultados,
    columnas        = columnas,
    h_periods       = h_periods,
    nivel_confianza = nivel_confianza,
    fecha_col       = fecha_col,
    timestamp       = Sys.time()
  )
  class(estructura) <- c("pronostico_racafe", "list")
  estructura
}


# ---- Imputacion de series de tiempo ----

#' Imputar valores faltantes en una serie de tiempo
#'
#' @param ts_data Vector numerico o serie `ts` con posibles `NA`.
#' @param metodo_imputacion Metodo a usar: `"promedio"`, `"mediana"`,
#'   `"interpolacion"`, `"constante"`, `"ultimo"`.
#' @param valor_constante Valor para el metodo `"constante"`.
#' @param prob_percentil Percentil para el metodo `"percentil"` (0-1).
#' @return Vector numerico sin `NA`.
#' @export
#' @examples
#' serie <- c(1, 2, NA, 4, NA, 6)
#' aplicar_imputacion(serie, "interpolacion")
#' aplicar_imputacion(serie, "promedio")
aplicar_imputacion <- function(
    ts_data,
    metodo_imputacion,
    valor_constante = NULL,
    prob_percentil  = 0.25) {

  metodos_validos <- c(
    "promedio", "mediana", "interpolacion",
    "constante", "ultimo", "percentil"
  )
  metodo_imputacion <- match.arg(metodo_imputacion, metodos_validos)

  if (!any(is.na(ts_data))) return(ts_data)

  switch(
    metodo_imputacion,
    promedio = {
      media <- mean(ts_data, na.rm = TRUE)
      ts_data[is.na(ts_data)] <- media
      ts_data
    },
    mediana = {
      med <- stats::median(ts_data, na.rm = TRUE)
      ts_data[is.na(ts_data)] <- med
      ts_data
    },
    interpolacion = {
      .check_pkg("zoo", "Pronosticos")
      as.numeric(zoo::na.approx(ts_data, na.rm = FALSE))
    },
    constante = {
      if (is.null(valor_constante)) {
        stop("Se requiere `valor_constante` para el metodo 'constante'.",
             call. = FALSE)
      }
      ts_data[is.na(ts_data)] <- valor_constante
      ts_data
    },
    ultimo = {
      .check_pkg("zoo", "Pronosticos")
      as.numeric(zoo::na.locf(ts_data, na.rm = FALSE))
    },
    percentil = {
      pct <- stats::quantile(ts_data, probs = prob_percentil, na.rm = TRUE)
      ts_data[is.na(ts_data)] <- pct
      ts_data
    }
  )
}


# ---- Extraccion de intervalos de confianza ----

#' Extraer intervalos de confianza de un objeto de pronostico
#'
#' @param forecast_obj Objeto de pronostico generado por el paquete `forecast`.
#' @param nivel_conf Nivel de confianza: `0.80` o `0.95`.
#' @return `tibble` con columnas `lower` y `upper`.
#' @export
extraer_intervalos <- function(forecast_obj, nivel_conf) {
  .check_pkg("forecast", "Pronosticos")

  nivel_idx <- which(abs(forecast_obj$level - nivel_conf * 100) < 1)

  if (length(nivel_idx) == 0) {
    stop(sprintf(
      "Nivel de confianza %.0f%% no disponible en el objeto de pronostico.",
      nivel_conf * 100
    ), call. = FALSE)
  }

  tibble::tibble(
    lower = as.numeric(forecast_obj$lower[, nivel_idx]),
    upper = as.numeric(forecast_obj$upper[, nivel_idx])
  )
}


# ---- Ajuste de multiples modelos ----

#' Ajustar multiples modelos y comparar resultados
#'
#' Ajusta ETS, ARIMA, NNETAR, TBATS y modelos de tendencia sobre los datos
#' de entrenamiento y evalua sobre los datos de prueba.
#'
#' @param train_data Vector numerico de entrenamiento.
#' @param test_data Vector numerico de prueba.
#' @param h_periods Horizonte de pronostico.
#' @param fechas_futuras Vector de fechas para el horizonte.
#' @param nivel_confianza Nivel de confianza para intervalos (0-1).
#' @param ... Argumentos adicionales (reservado para extensiones).
#' @return Lista con resultados por modelo.
#' @export
ejecutar_pronosticos <- function(
    train_data,
    test_data,
    h_periods,
    fechas_futuras,
    nivel_confianza = 0.95,
    ...) {

  .check_pkg("forecast", "Pronosticos")

  ts_train <- stats::ts(train_data)
  n_test   <- length(test_data)
  niveles  <- c(80, as.integer(nivel_confianza * 100))
  niveles  <- unique(niveles)

  modelos <- list(
    ETS = function(x) {
      mod <- forecast::ets(x)
      forecast::forecast(mod, h = h_periods, level = niveles)
    },
    ARIMA = function(x) {
      mod <- forecast::auto.arima(x)
      forecast::forecast(mod, h = h_periods, level = niveles)
    },
    NNETAR = function(x) {
      mod <- forecast::nnetar(x)
      forecast::forecast(mod, h = h_periods, level = niveles)
    },
    TBATS = function(x) {
      mod <- forecast::tbats(x)
      forecast::forecast(mod, h = h_periods, level = niveles)
    },
    Tendencia = function(x) {
      t_idx <- seq_along(x)
      modelo_lm <- stats::lm(x ~ t_idx)
      t_fut     <- seq(length(x) + 1, length(x) + h_periods)
      pred      <- stats::predict(
        modelo_lm,
        newdata  = data.frame(t_idx = t_fut),
        interval = "prediction",
        level    = nivel_confianza
      )
      # Emular estructura forecast
      list(
        mean  = pred[, "fit"],
        lower = matrix(pred[, "lwr"], ncol = 1),
        upper = matrix(pred[, "upr"], ncol = 1),
        level = as.integer(nivel_confianza * 100)
      )
    }
  )

  resultados <- purrr::imap(modelos, function(fn_modelo, nombre) {
    tryCatch(
      {
        fc <- fn_modelo(ts_train)
        vals_fc <- as.numeric(fc$mean)
        n_eval  <- min(n_test, length(vals_fc))

        metricas <- .calcular_metricas(
          real      = test_data[seq_len(n_eval)],
          pronostico = vals_fc[seq_len(n_eval)]
        )

        intervalos <- tryCatch(
          extraer_intervalos(fc, nivel_confianza),
          error = function(e) {
            tibble::tibble(
              lower = rep(NA_real_, h_periods),
              upper = rep(NA_real_, h_periods)
            )
          }
        )

        tibble::tibble(
          modelo          = nombre,
          fecha           = fechas_futuras,
          pronostico      = vals_fc[seq_len(h_periods)],
          lower           = intervalos$lower,
          upper           = intervalos$upper,
          rmse            = metricas$rmse,
          mae             = metricas$mae,
          mape            = metricas$mape,
          error           = FALSE,
          msg_error       = NA_character_
        )
      },
      error = function(e) {
        tibble::tibble(
          modelo     = nombre,
          fecha      = fechas_futuras,
          pronostico = rep(NA_real_, h_periods),
          lower      = rep(NA_real_, h_periods),
          upper      = rep(NA_real_, h_periods),
          rmse       = NA_real_,
          mae        = NA_real_,
          mape       = NA_real_,
          error      = TRUE,
          msg_error  = conditionMessage(e)
        )
      }
    )
  })

  dplyr::bind_rows(resultados)
}


# ---- Orquestador principal ----

#' Orquestar el flujo completo de pronosticos
#'
#' Divide la serie en train/test, imputa NA, ajusta multiples modelos
#' y retorna un objeto `pronostico_racafe` estandarizado.
#'
#' @param df `data.frame` con la serie de tiempo.
#' @param fecha_col Nombre de la columna de fechas.
#' @param valor_cols Vector de nombres de columnas a pronosticar.
#'   `NULL` selecciona todas las columnas numericas excepto la fecha.
#' @param nivel_confianza Nivel de confianza para intervalos (0-1).
#' @param prop_train Proporcion de datos para entrenamiento (0-1).
#' @param h_periods Horizonte de pronostico en periodos.
#' @param metodo_imputacion Metodo de imputacion de NA.
#' @param ... Argumentos adicionales para `ejecutar_pronosticos`.
#' @return Objeto `pronostico_racafe`.
#' @export
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     fecha  = seq(as.Date("2022-01-01"), by = "month", length.out = 36),
#'     ventas = cumsum(rnorm(36, mean = 100, sd = 15))
#'   )
#'   pron <- Pronosticar(df, fecha_col = "fecha", valor_cols = "ventas")
#' }
Pronosticar <- function(
    df,
    fecha_col       = "fecha",
    valor_cols      = NULL,
    nivel_confianza = 0.95,
    prop_train      = 0.8,
    h_periods       = 12,
    metodo_imputacion = "interpolacion",
    ...) {

  .check_pkg("forecast", "Pronosticos")

  stopifnot(
    is.data.frame(df),
    fecha_col %in% names(df),
    prop_train > 0 && prop_train < 1,
    nivel_confianza > 0 && nivel_confianza < 1
  )

  # Seleccion de columnas a pronosticar
  if (is.null(valor_cols)) {
    valor_cols <- names(df)[sapply(df, is.numeric)]
    valor_cols <- setdiff(valor_cols, fecha_col)
  }

  if (length(valor_cols) == 0) {
    stop("No se encontraron columnas numericas para pronosticar.", call. = FALSE)
  }

  df <- df |>
    dplyr::arrange(dplyr::across(dplyr::all_of(fecha_col)))

  n_total  <- nrow(df)
  n_train  <- floor(n_total * prop_train)
  n_test   <- n_total - n_train

  fechas_futuras <- .generar_fechas_futuras(
    df[[fecha_col]], h_periods
  )

  resultados <- purrr::set_names(
    purrr::map(valor_cols, function(col) {
      serie <- df[[col]]
      serie <- aplicar_imputacion(serie, metodo_imputacion)

      train <- serie[seq_len(n_train)]
      test  <- serie[seq(n_train + 1, n_total)]

      ejecutar_pronosticos(
        train_data      = train,
        test_data       = test,
        h_periods       = h_periods,
        fechas_futuras  = fechas_futuras,
        nivel_confianza = nivel_confianza,
        ...
      )
    }),
    valor_cols
  )

  .nuevo_resultado_pronostico(
    resultados      = resultados,
    columnas        = valor_cols,
    h_periods       = h_periods,
    nivel_confianza = nivel_confianza,
    fecha_col       = fecha_col
  )
}


# ---- Analisis de resultados ----

#' Calcular metricas de precision por columna
#'
#' @param resultado_pronostico Objeto `pronostico_racafe`.
#' @param columna Columna a evaluar. `NULL` evalua todas.
#' @return `tibble` con RMSE, MAE y MAPE por modelo y columna.
#' @export
PronMetricas <- function(resultado_pronostico, columna = NULL) {
  .validar_resultado(resultado_pronostico)

  cols <- columna %||% resultado_pronostico$columnas

  purrr::map_dfr(cols, function(col) {
    resultado_pronostico$resultados[[col]] |>
      dplyr::filter(!.data$error) |>
      dplyr::select(.data$modelo, .data$rmse, .data$mae, .data$mape) |>
      dplyr::distinct() |>
      dplyr::mutate(columna = col)
  })
}


#' Seleccionar el mejor modelo por columna
#'
#' Elige el modelo con menor RMSE que no haya fallado.
#'
#' @param resultado_pronostico Objeto `pronostico_racafe`.
#' @param columna Columna a evaluar. `NULL` evalua todas.
#' @param ... Reservado para extensiones (criterio de seleccion).
#' @return Objeto `pronostico_racafe` filtrado al mejor modelo por columna.
#' @export
PronSeleccionar <- function(resultado_pronostico, columna = NULL, ...) {
  .validar_resultado(resultado_pronostico)

  cols <- columna %||% resultado_pronostico$columnas

  seleccion <- purrr::set_names(
    purrr::map(cols, function(col) {
      df_col <- resultado_pronostico$resultados[[col]]

      mejor <- df_col |>
        dplyr::filter(!.data$error, !is.na(.data$rmse)) |>
        dplyr::group_by(.data$modelo) |>
        dplyr::slice(1) |>
        dplyr::ungroup() |>
        dplyr::slice_min(.data$rmse, n = 1)

      if (nrow(mejor) == 0) {
        stop(sprintf("Ningun modelo convergio para la columna '%s'.", col),
             call. = FALSE)
      }

      modelo_ganador <- mejor$modelo[[1]]
      df_col |> dplyr::filter(.data$modelo == modelo_ganador)
    }),
    cols
  )

  resultado_pronostico$resultados <- seleccion
  resultado_pronostico$columnas   <- cols
  resultado_pronostico
}


#' Construir tabla detallada de serie pronosticada
#'
#' @param seleccion Objeto `pronostico_racafe` post-seleccion.
#' @param columna Columna. `NULL` combina todas.
#' @return `tibble` con fecha, pronostico e intervalos.
#' @export
PronSerie <- function(seleccion, columna = NULL) {
  .validar_resultado(seleccion)
  cols <- columna %||% seleccion$columnas

  purrr::map_dfr(cols, function(col) {
    seleccion$resultados[[col]] |>
      dplyr::select(
        .data$modelo, .data$fecha, .data$pronostico,
        .data$lower, .data$upper
      ) |>
      dplyr::mutate(columna = col)
  })
}


#' Resumir pronosticos por mes
#'
#' @param seleccion Objeto `pronostico_racafe` post-seleccion.
#' @param columna Columna. `NULL` combina todas.
#' @param incluir_pronosticos Logico. Incluir columna de pronostico puntual.
#' @return `tibble` con resumen mensual.
#' @export
PronMensual <- function(seleccion, columna = NULL, incluir_pronosticos = TRUE) {
  serie <- PronSerie(seleccion, columna)

  serie <- serie |>
    dplyr::mutate(
      anio = lubridate::year(.data$fecha),
      mes  = lubridate::month(.data$fecha)
    ) |>
    dplyr::group_by(.data$columna, .data$modelo, .data$anio, .data$mes) |>
    dplyr::summarise(
      pronostico = sum(.data$pronostico, na.rm = TRUE),
      lower      = sum(.data$lower, na.rm = TRUE),
      upper      = sum(.data$upper, na.rm = TRUE),
      .groups    = "drop"
    )

  if (!incluir_pronosticos) {
    serie <- dplyr::select(serie, -.data$pronostico)
  }

  serie
}


#' Analizar patron mensual de comportamiento
#'
#' Calcula el indice estacional promedio por mes.
#'
#' @param seleccion Objeto `pronostico_racafe` post-seleccion.
#' @param columna Columna. `NULL` combina todas.
#' @return `tibble` con patron mensual (indice promedio por mes).
#' @export
PronPatronMes <- function(seleccion, columna = NULL) {
  serie <- PronSerie(seleccion, columna)

  serie |>
    dplyr::mutate(mes = lubridate::month(.data$fecha, label = TRUE)) |>
    dplyr::group_by(.data$columna, .data$mes) |>
    dplyr::summarise(
      promedio = mean(.data$pronostico, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::group_by(.data$columna) |>
    dplyr::mutate(
      indice = .data$promedio / mean(.data$promedio, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}


# ---- Utilidades internas de pronosticos ----

# Calcular metricas de error punto a punto
.calcular_metricas <- function(real, pronostico) {
  n    <- length(real)
  err  <- real - pronostico
  rmse <- sqrt(mean(err^2, na.rm = TRUE))
  mae  <- mean(abs(err), na.rm = TRUE)
  mape <- mean(
    abs(err / ifelse(real == 0, NA_real_, real)),
    na.rm = TRUE
  ) * 100

  list(rmse = rmse, mae = mae, mape = mape)
}

# Generar vector de fechas futuras basado en la frecuencia detectada
.generar_fechas_futuras <- function(fechas, h) {
  fechas <- sort(as.Date(fechas))
  n      <- length(fechas)
  if (n < 2) return(seq.Date(max(fechas), by = "month", length.out = h + 1)[-1])

  diffs   <- as.integer(diff(fechas))
  periodo <- as.integer(stats::median(diffs))

  seq.Date(max(fechas) + periodo, by = periodo, length.out = h)
}

# Validar que el objeto es pronostico_racafe
.validar_resultado <- function(x) {
  if (!inherits(x, "pronostico_racafe")) {
    stop(
      "Se esperaba un objeto `pronostico_racafe`. ",
      "Usar la salida de `Pronosticar()` o `PronSeleccionar()`.",
      call. = FALSE
    )
  }
}
