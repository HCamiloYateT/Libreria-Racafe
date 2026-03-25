# ============================================================
# Seccion 03 — Transformacion de datos
# Top, joins, duplicados, operadores y helpers de data frames
# ============================================================

.validar_data_frame <- function(x, arg = "data") {
  if (!is.data.frame(x)) {
    stop(sprintf("`%s` debe ser un data.frame.", arg), call. = FALSE)
  }
}

.validar_string_unico <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || trimws(x) == "") {
    stop(sprintf("`%s` debe ser un string no vacio de longitud 1.", arg), call. = FALSE)
  }
}

.validar_columnas_existentes <- function(data, cols, arg_cols = "by", arg_data = "data") {
  faltantes <- setdiff(cols, names(data))
  if (length(faltantes) > 0) {
    stop(
      sprintf(
        "Columnas no encontradas en `%s` para `%s`: %s.",
        arg_data, arg_cols, paste(faltantes, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# ---- Recodificacion de categorias ----

#' Recodificar categorias poco frecuentes (funcion unificada)
#'
#' Agrupa en una categoria "otros" las categorias que no superan
#' el umbral definido por la estrategia seleccionada.
#'
#' @param data `data.frame` o `tibble`.
#' @param var_recode Variable categorica a recodificar (tidy-select).
#' @param var_top Variable numerica para el calculo del ranking.
#' @param fun_Top Funcion de resumen: `"sum"`, `"n"` o `"mean"`.
#' @param estrategia `"absoluto"` (top N) o `"relativo"` (porcentaje minimo).
#' @param n Numero de categorias a conservar cuando `estrategia = "absoluto"`.
#' @param pct_min Porcentaje minimo cuando `estrategia = "relativo"` (0-1).
#' @param nom_var Nombre de la nueva columna recodificada.
#' @param lab_recodificar Etiqueta para las categorias agrupadas.
#' @return `data.frame` con la nueva columna agregada.
#' @export
#' @examples
#' df <- data.frame(
#'   categoria = c("A","A","B","B","C","D"),
#'   valor = c(10, 20, 5, 15, 2, 1)
#' )
#' RecodificarTop(df, categoria, valor, "sum", "absoluto",
#'   n = 2, nom_var = "cat_top")
RecodificarTop <- function(
    data,
    var_recode,
    var_top,
    fun_Top,
    estrategia      = c("absoluto", "relativo"),
    n               = 10,
    pct_min         = 0.05,
    nom_var,
    lab_recodificar = "OTROS") {
  .validar_data_frame(data, "data")
  .validar_string_unico(fun_Top, "fun_Top")
  .validar_string_unico(nom_var, "nom_var")
  .validar_string_unico(lab_recodificar, "lab_recodificar")

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 || n %% 1 != 0) {
    stop("`n` debe ser un entero mayor o igual a 1.", call. = FALSE)
  }
  if (!is.numeric(pct_min) || length(pct_min) != 1 || is.na(pct_min) || pct_min < 0 || pct_min > 1) {
    stop("`pct_min` debe ser numerico en el rango [0, 1].", call. = FALSE)
  }

  estrategia <- match.arg(estrategia)

  fn <- switch(
    fun_Top,
    sum  = function(x) sum(x, na.rm = TRUE),
    n    = function(x) dplyr::n(),
    mean = function(x) mean(x, na.rm = TRUE),
    stop(sprintf("fun_Top debe ser 'sum', 'n' o 'mean', no '%s'.", fun_Top),
         call. = FALSE)
  )

  resumen <- data |>
    dplyr::summarise(
      .total = fn({{ var_top }}),
      .by    = {{ var_recode }}
    ) |>
    dplyr::arrange(dplyr::desc(.data$.total))

  mantener <- if (estrategia == "absoluto") {
    dplyr::slice_head(resumen, n = n) |>
      dplyr::pull({{ var_recode }})
  } else {
    total_global <- sum(resumen$.total, na.rm = TRUE)
    resumen |>
      dplyr::filter(.data$.total / total_global >= pct_min) |>
      dplyr::pull({{ var_recode }})
  }

  data |>
    dplyr::mutate(
      "{nom_var}" := dplyr::if_else(
        {{ var_recode }} %in% mantener,
        as.character({{ var_recode }}),
        lab_recodificar
      )
    )
}


#' Recodificar por top absoluto (N categorias mas frecuentes)
#'
#' Wrapper de `RecodificarTop()` con estrategia `"absoluto"`.
#'
#' @inheritParams RecodificarTop
#' @export
TopAbsoluto <- function(
    data,
    var_recode,
    var_top,
    fun_Top,
    n               = 10,
    nom_var,
    lab_recodificar = "OTROS") {

  RecodificarTop(
    data           = data,
    var_recode     = {{ var_recode }},
    var_top        = {{ var_top }},
    fun_Top        = fun_Top,
    estrategia     = "absoluto",
    n              = n,
    nom_var        = nom_var,
    lab_recodificar = lab_recodificar
  )
}


#' Recodificar por umbral relativo (porcentaje minimo)
#'
#' Wrapper de `RecodificarTop()` con estrategia `"relativo"`.
#'
#' @inheritParams RecodificarTop
#' @export
TopRelativo <- function(
    data,
    var_recode,
    var_top,
    fun_Top,
    pct_min         = 0.05,
    nom_var,
    lab_recodificar = "OTROS") {

  RecodificarTop(
    data            = data,
    var_recode      = {{ var_recode }},
    var_top         = {{ var_top }},
    fun_Top         = fun_Top,
    estrategia      = "relativo",
    pct_min         = pct_min,
    nom_var         = nom_var,
    lab_recodificar = lab_recodificar
  )
}


# ---- Manipulacion de tablas ----

#' Agregar columnas con botones HTML interactivos
#'
#' @param tabla `data.frame` al que se agregan los botones.
#' @param botones Vector de caracteres con los nombres de los botones.
#' @return `data.frame` con nuevas columnas HTML.
#' @export
AdicionarBotones <- function(tabla, botones) {
  .validar_data_frame(tabla, "tabla")
  if (!is.character(botones) || length(botones) == 0 || any(is.na(botones)) || any(trimws(botones) == "")) {
    stop("`botones` debe ser un vector character no vacio sin NA ni cadenas vacias.", call. = FALSE)
  }

  for (boton in botones) {
    col_nombre <- janitor::make_clean_names(boton)
    tabla[[col_nombre]] <- sprintf(
      '<button class="btn btn-sm btn-outline-secondary" data-accion="%s">%s</button>',
      boton, boton
    )
  }
  tabla
}


#' Combinar data.frames ignorando los vacios
#'
#' Equivalente a `dplyr::bind_rows()` pero omite `data.frame`s con
#' cero filas o `NULL` sin generar advertencias.
#'
#' @param ... `data.frame`s a combinar.
#' @return `data.frame` combinado.
#' @export
#' @examples
#' df1 <- data.frame(x = 1:2)
#' df2 <- data.frame(x = integer(0))
#' df3 <- data.frame(x = 3:4)
#' bind_rows_na(df1, df2, df3)
bind_rows_na <- function(...) {
  lista <- list(...)
  invalidos <- which(!vapply(lista, function(x) is.null(x) || is.data.frame(x), logical(1)))
  if (length(invalidos) > 0) {
    stop(
      sprintf("Todos los argumentos deben ser data.frame o NULL. Invalidos en posiciones: %s.", paste(invalidos, collapse = ", ")),
      call. = FALSE
    )
  }
  lista_valida <- Filter(
    function(df) !is.null(df) && is.data.frame(df) && nrow(df) > 0,
    lista
  )
  if (length(lista_valida) == 0) return(data.frame())
  dplyr::bind_rows(lista_valida)
}


#' Joins iterativos sobre una lista de data.frames
#'
#' @param x `data.frame` base.
#' @param y_list Lista de `data.frame`s a unir.
#' @param by Columnas de union (mismo formato que en `dplyr::*_join`).
#' @param type Tipo de join: `"left"`, `"inner"`, `"full"`, `"right"`.
#' @return `data.frame` resultado de los joins encadenados.
#' @export
#' @examples
#' base <- data.frame(id = 1:3)
#' extra1 <- data.frame(id = 1:2, a = c("x", "y"))
#' extra2 <- data.frame(id = 2:3, b = c("p", "q"))
#' left_join_all(base, list(extra1, extra2), by = "id")
left_join_all <- function(x, y_list, by, type = "left") {
  .validar_data_frame(x, "x")
  if (!is.list(y_list) || length(y_list) == 0) {
    stop("`y_list` debe ser una lista no vacia de data.frames.", call. = FALSE)
  }
  if (!all(vapply(y_list, is.data.frame, logical(1)))) {
    stop("`y_list` debe contener solo data.frames.", call. = FALSE)
  }
  if (!is.character(by) || length(by) < 1 || any(is.na(by)) || any(trimws(by) == "")) {
    stop("`by` debe ser un vector character no vacio con nombres de columnas.", call. = FALSE)
  }
  .validar_columnas_existentes(x, by, arg_cols = "by", arg_data = "x")
  for (i in seq_along(y_list)) {
    .validar_columnas_existentes(y_list[[i]], by, arg_cols = "by", arg_data = sprintf("y_list[[%d]]", i))
  }

  join_fn <- switch(
    type,
    left  = dplyr::left_join,
    inner = dplyr::inner_join,
    full  = dplyr::full_join,
    right = dplyr::right_join,
    stop(sprintf("type '%s' no valido. Usar: left, inner, full, right.", type),
         call. = FALSE)
  )

  purrr::reduce(y_list, function(acc, y) join_fn(acc, y, by = by), .init = x)
}


#' Revisar llaves duplicadas entre dos tablas
#'
#' @param x Primera tabla.
#' @param y Segunda tabla.
#' @param by Columnas de llave para la comparacion.
#' @return Lista con dos elementos: `duplicados_x` y `duplicados_y`.
#' @export
#' @examples
#' a <- data.frame(id = c(1, 1, 2))
#' b <- data.frame(id = c(1, 2, 2))
#' RevisarDuplicados(a, b, by = "id")
RevisarDuplicados <- function(x, y, by) {
  .validar_data_frame(x, "x")
  .validar_data_frame(y, "y")
  if (!is.character(by) || length(by) < 1 || any(is.na(by)) || any(trimws(by) == "")) {
    stop("`by` debe ser un vector character no vacio con nombres de columnas.", call. = FALSE)
  }
  .validar_columnas_existentes(x, by, arg_cols = "by", arg_data = "x")
  .validar_columnas_existentes(y, by, arg_cols = "by", arg_data = "y")

  dup_x <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  dup_y <- y |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(dup_x) == 0 && nrow(dup_y) == 0) {
    message("Sin duplicados en ninguna tabla.")
  } else {
    if (nrow(dup_x) > 0) message(sprintf("%d duplicados en tabla x.", nrow(dup_x)))
    if (nrow(dup_y) > 0) message(sprintf("%d duplicados en tabla y.", nrow(dup_y)))
  }

  list(duplicados_x = dup_x, duplicados_y = dup_y)
}


# ---- Operadores ----

#' Operador null-coalescing
#'
#' Retorna `a` si no es `NULL` ni vector de longitud 0; de lo contrario `b`.
#'
#' @param a Valor principal.
#' @param b Valor por defecto.
#' @return `a` o `b`.
#' @export
#' @examples
#' NULL %||% "valor_por_defecto"
#' "" %||% "fallback"
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}
