# Reglas de asociación sobre cestas preparadas por el consumidor ----
.mba_dividir <- function(a, b) {
  if (!length(a) || !length(b)) return(numeric())
  b <- rep_len(b, max(length(a), length(b)))
  ifelse(is.finite(b) & b > 0, a / b, NA_real_)
}

.mba_clave <- function(items) paste(sort(items), collapse = ",")

#' ConjuntosFrecuentes: componente reutilizable del ecosistema Racafé
#' @export
ConjuntosFrecuentes <- function(membresia, cestas, soporte = 0.03, minimo = 3L, longitud = 3L, limite = 50000L) {
  n <- length(cestas)
  if (!is.finite(soporte) || soporte < 0 || soporte > 1 || !is.finite(minimo) || minimo < 1 ||
      !longitud %in% 2:5) stop("Revise soporte, mínimo de cestas y longitud (2 a 5).")
  if (!n) return(list(conjuntos = list(), n = 0L, minimo = minimo, candidatos = 0L))
  umbral <- max(ceiling(minimo), ceiling(soporte * n - 1e-10))
  vertical <- split(membresia$Cesta, membresia$Item)
  vertical <- lapply(vertical, function(ids) sort(unique(ids[ids %in% cestas])))
  vertical <- vertical[lengths(vertical) >= umbral]
  conjuntos <- lapply(names(vertical), function(id) as.integer(id))
  nombres <- vapply(conjuntos, .mba_clave, character(1))
  todos <- stats::setNames(lapply(seq_along(conjuntos), function(i) {
    list(items = conjuntos[[i]], tids = vertical[[i]], cuenta = length(vertical[[i]]))
  }), nombres)
  previos <- todos
  candidatos_totales <- 0L
  for (k in 2:longitud) {
    if (length(previos) < 2L) break
    grupos <- split(seq_along(previos), vapply(previos, function(a) .mba_clave(head(a$items, k - 2L)), character(1)))
    generados <- list()
    for (indices in grupos) {
      if (length(indices) < 2L) next
      posibles <- length(indices) * (length(indices) - 1) / 2
      if (candidatos_totales + posibles > limite) {
        stop(paste("Se alcanzó el límite de candidatos. Aumente el soporte o reduzca la longitud;",
          "no se muestran reglas parciales."))
      }
      pares <- utils::combn(indices, 2L)
      for (j in seq_len(ncol(pares))) {
        a <- previos[[pares[1, j]]]; b <- previos[[pares[2, j]]]
        union <- sort(unique(c(a$items, b$items)))
        if (length(union) != k) next
        candidatos_totales <- candidatos_totales + 1L
        subconjuntos <- utils::combn(union, k - 1L, simplify = FALSE)
        if (!all(vapply(subconjuntos, .mba_clave, character(1)) %in% names(previos))) next
        tids <- intersect(a$tids, b$tids)
        if (length(tids) >= umbral) generados[[.mba_clave(union)]] <- list(items = union,
          tids = tids, cuenta = length(tids))
      }
    }
    if (!length(generados)) break
    todos <- c(todos, generados)
    previos <- generados
  }
  list(conjuntos = todos, n = n, minimo = umbral, candidatos = candidatos_totales)
}

#' MetricasAsociacion: componente reutilizable del ecosistema Racafé
#' @export
MetricasAsociacion <- function(n, a, b, ab) {
  pa <- a / n; pb <- b / n; pab <- ab / n
  confianza <- .mba_dividir(ab, a)
  denominador <- max(pab * (1 - pa), pa * (pb - pab))
  conviccion <- if (is.na(confianza)) NA_real_ else if (confianza == 1) {
    if (pb < 1) Inf else NA_real_
  } else (1 - pb) / (1 - confianza)
  c(Soporte = pab, Confianza = confianza, Lift = .mba_dividir(confianza, pb),
    Leverage = pab - pa * pb, Conviccion = conviccion, Jaccard = .mba_dividir(ab, a + b - ab),
    Coseno = .mba_dividir(ab, sqrt(a * b)), Zhang = .mba_dividir(pab - pa * pb, denominador))
}

#' ReglasAsociacion: componente reutilizable del ecosistema Racafé
#' @export
ReglasAsociacion <- function(modelo, confianza = 0.5, lift = 1, estadistica = TRUE, limite = 20000L) {
  conjuntos <- modelo$conjuntos
  vacio <- data.frame(Regla = integer(), Antecedente = character(), Consecuente = character(),
    CestasAmbos = integer(), Soporte = numeric(), Confianza = numeric(), Lift = numeric())
  if (!length(conjuntos)) return(vacio)
  familias <- Filter(function(x) length(x$items) > 1L, conjuntos)
  total <- sum(vapply(familias, function(x) 2^length(x$items) - 2L, numeric(1)))
  if (total > limite) stop("Hay demasiadas reglas candidatas. Suba el soporte o reduzca la longitud.")
  filas <- list()
  for (conjunto in familias) {
    for (tam in seq_len(length(conjunto$items) - 1L)) {
      antecedentes <- utils::combn(conjunto$items, tam, simplify = FALSE)
      for (a in antecedentes) {
        b <- setdiff(conjunto$items, a)
        na <- conjuntos[[.mba_clave(a)]]$cuenta
        nb <- conjuntos[[.mba_clave(b)]]$cuenta
        fila <- data.frame(Antecedente = .mba_clave(a), Consecuente = .mba_clave(b),
          CestasAntecedente = na, CestasConsecuente = nb, CestasAmbos = conjunto$cuenta)
        met <- MetricasAsociacion(modelo$n, na, nb, conjunto$cuenta)
        for (nombre in names(met)) fila[[nombre]] <- met[[nombre]]
        filas[[length(filas) + 1L]] <- fila
      }
    }
  }
  if (!length(filas)) return(vacio)
  reglas <- dplyr::bind_rows(filas)
  reglas$Pvalor <- reglas$Qvalor <- rep(NA_real_, nrow(reglas))
  if (estadistica) {
    particiones <- vapply(seq_len(nrow(reglas)), function(i) {
      paste(sort(c(reglas$Antecedente[[i]], reglas$Consecuente[[i]])), collapse = "|")
    }, character(1))
    unicas <- which(!duplicated(particiones))
    valores <- vapply(unicas, function(i) {
      a <- reglas$CestasAntecedente[[i]]; b <- reglas$CestasConsecuente[[i]]; ab <- reglas$CestasAmbos[[i]]
      tabla <- matrix(c(ab, a - ab, b - ab, modelo$n - a - b + ab), 2L)
      if (any(rowSums(tabla) == 0) || any(colSums(tabla) == 0)) return(NA_real_)
      stats::fisher.test(tabla, alternative = "greater")$p.value
    }, numeric(1))
    reglas$Pvalor <- valores[match(particiones, particiones[unicas])]
    reglas$Qvalor <- stats::p.adjust(valores, method = "BH")[match(particiones, particiones[unicas])]
  }
  # BH se calcula sobre toda la familia antes de filtrar por confianza/lift.
  reglas <- reglas[is.finite(reglas$Confianza) & reglas$Confianza >= confianza &
    is.finite(reglas$Lift) & reglas$Lift >= lift, , drop = FALSE]
  reglas <- reglas[order(-reglas$Lift, -reglas$Confianza, -reglas$CestasAmbos), ]
  reglas$Regla <- seq_len(nrow(reglas))
  reglas
}

#' ValidarReglasAsociacion: componente reutilizable del ecosistema Racafé
#' @export
ValidarReglasAsociacion <- function(reglas, membresia, cestas) {
  n <- length(cestas)
  vertical <- split(membresia$Cesta, membresia$Item)
  contar <- function(clave) {
    items <- strsplit(clave, ",", fixed = TRUE)[[1]]
    listas <- lapply(items, function(item) intersect(vertical[[item]], cestas))
    if (!length(listas)) integer() else Reduce(intersect, listas)
  }
  salida <- reglas
  salida$SoporteValidacion <- salida$ConfianzaValidacion <- salida$LiftValidacion <- rep(NA_real_, nrow(salida))
  salida$AntecedentesValidacion <- salida$ConsecuentesValidacion <- salida$AmbosValidacion <- rep(0L, nrow(salida))
  if (!n) return(salida)
  for (i in seq_len(nrow(salida))) {
    a <- contar(salida$Antecedente[[i]]); b <- contar(salida$Consecuente[[i]])
    ab <- length(intersect(a, b))
    met <- MetricasAsociacion(n, length(a), length(b), ab)
    salida$AntecedentesValidacion[[i]] <- length(a)
    salida$ConsecuentesValidacion[[i]] <- length(b)
    salida$AmbosValidacion[[i]] <- ab
    salida$SoporteValidacion[[i]] <- met[["Soporte"]]
    salida$ConfianzaValidacion[[i]] <- met[["Confianza"]]
    salida$LiftValidacion[[i]] <- met[["Lift"]]
  }
  salida
}

