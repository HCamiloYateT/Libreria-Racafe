# Modelo BG/NBD sobre frecuencia, recencia y exposición semanales ----
.btyd_logsum <- function(a, b) { m <- pmax(a, b); m + log(exp(a - m) + exp(b - m)) }

.btyd_ll <- function(parametros, x) {
  p <- exp(parametros); r <- p[1]; alpha <- p[2]; a <- p[3]; b <- p[4]
  f <- x$Frecuencia
  A1 <- lgamma(r + f) - lgamma(r) + r * log(alpha)
  A2 <- lbeta(a, b + f) - lbeta(a, b)
  vivo <- -(r + f) * log(alpha + x$T)
  muerto <- rep(-Inf, length(f)); i <- f > 0
  muerto[i] <- log(a) - log(b + f[i] - 1) - (r + f[i]) * log(alpha + x$tx[i])
  -sum(A1 + A2 + .btyd_logsum(vivo, muerto))
}

#' AjustarBGNBD: componente reutilizable del ecosistema Racafé
#' @export
AjustarBGNBD <- function(x) {
  if (nrow(x) < 20L || sum(x$Frecuencia > 0) < 5L) {
    stop("BG/NBD requiere al menos 20 clientes/línea y cinco con recompra para ajustar la población.")
  }
  inicios <- list(c(1, 10, 1, 3), c(0.5, 30, 2, 5), c(2, 50, 0.5, 2))
  ajustes <- lapply(inicios, function(p) tryCatch(stats::optim(log(p), .btyd_ll, x = x,
    method = "L-BFGS-B", lower = rep(log(1e-4), 4), upper = rep(log(1e5), 4)), error = function(e) NULL))
  ajustes <- Filter(function(a) !is.null(a) && a$convergence == 0L && is.finite(a$value), ajustes)
  if (!length(ajustes)) stop("BG/NBD no convergió. Amplíe el historial o revise los datos de compras.")
  mejor <- ajustes[[which.min(vapply(ajustes, function(a) a$value, numeric(1)))]]
  list(parametros = stats::setNames(exp(mejor$par), c("r", "alpha", "a", "b")),
    LogVerosimilitud = -mejor$value, Clientes = nrow(x), Convergencia = mejor$convergence,
    EnLimite = any(abs(mejor$par - log(1e-4)) < 0.01 | abs(mejor$par - log(1e5)) < 0.01))
}

#' PredecirBGNBD: componente reutilizable del ecosistema Racafé
#' @export
PredecirBGNBD <- function(x, modelo, semanas) {
  p <- modelo$parametros; r <- p[["r"]]; alpha <- p[["alpha"]]; a <- p[["a"]]; b <- p[["b"]]
  log_odds <- rep(-Inf, nrow(x)); i <- x$Frecuencia > 0
  log_odds[i] <- log(a) - log(b + x$Frecuencia[i] - 1) +
    (r + x$Frecuencia[i]) * log((alpha + x$T[i]) / (alpha + x$tx[i]))
  x$ProbActivo <- stats::plogis(-log_odds)
  # Expectativa condicional integrada sobre la propensión de abandono; semanas es la unidad del ajuste.
  x$ComprasEsperadas <- vapply(seq_len(nrow(x)), function(i) {
    shape <- r + x$Frecuencia[i]; rate <- alpha + x$T[i]
    integral <- stats::integrate(function(q) {
      abandono <- stats::qbeta(q, a, b + x$Frecuencia[i])
      valor <- -expm1(-shape * log1p(semanas * abandono / rate)) / abandono
      valor[abandono == 0] <- shape * semanas / rate
      valor
    }, 0, 1, rel.tol = 1e-5, subdivisions = 200L)$value
    x$ProbActivo[i] * integral
  }, numeric(1))
  x
}

