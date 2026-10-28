# Propuesta de adaptador OTP; no sustituye scripts de FastAPI ----
# Entrada estándar UTF-8: destino, código y nombre, una línea por campo.
# Ejecutar este script directamente ENVÍA correo. Hacer source() no lo ejecuta.
.rmail_stdin_main <- function() {
  connection <- file("stdin", open = "r", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  fields <- readLines(connection, n = 4L, warn = FALSE)
  if (length(fields) != 3L) stop("Entrada OTP no valida.", call. = FALSE)
  racafeMail::racafeMailOTP(para = fields[[1]], codigo = fields[[2]], nombre = fields[[3]])
}
if (sys.nframe() == 0L) {
  status <- tryCatch({ .rmail_stdin_main(); 0L }, error = function(e) {
    cat("No se pudo completar el envio OTP.\n", file = stderr())
    1L
  })
  quit(save = "no", status = status)
}
