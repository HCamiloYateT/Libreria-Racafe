# ============================================================
# Seccion 02 — Utilidades Git
# Inicializacion y publicacion inicial de proyectos
# ============================================================

#' Realizar el primer push forzado de un proyecto a GitHub
#'
#' Reinicia la configuracion Git local del proyecto actual, descarga el
#' `.gitignore` corporativo, crea el commit inicial y lo envia a la rama
#' `main` del repositorio remoto indicado.
#'
#' @param repo_github URL del repositorio remoto de GitHub.
#' @return Invisiblemente, el codigo de estado retornado por `git push`.
#' @export
PushInicial <- function(repo_github) {
  if (!is.character(repo_github) || length(repo_github) != 1 || is.na(repo_github) || trimws(repo_github) == "") {
    stop("`repo_github` debe ser un string no vacio de longitud 1.", call. = FALSE)
  }

  if (requireNamespace("here", quietly = TRUE)) {
    ruta_proyecto <- getExportedValue("here", "here")()
  } else {
    ruta_proyecto <- getwd()
  }
  gitignore_url <- "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/git/.gitignore"
  setwd(ruta_proyecto)

  message("Proyecto actual: ", getwd())

  top_git <- tryCatch(
    suppressWarnings(system2(
      "git",
      c("rev-parse", "--show-toplevel"),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) NULL
  )

  if (!is.null(top_git) && length(top_git) > 0) {
    top_git <- top_git[1]

    if (normalizePath(top_git, mustWork = FALSE) != normalizePath(getwd(), mustWork = FALSE)) {
      unlink(file.path(top_git, ".git"), recursive = TRUE, force = TRUE)
      message("Git anterior eliminado: ", top_git)
    }
  }

  if (dir.exists(".git")) {
    unlink(".git", recursive = TRUE, force = TRUE)
    message("Repositorio Git local reiniciado")
  }

  system2("git", "init")
  Sys.chmod(path.expand("~/.ssh"), "700")

  llave_ssh <- path.expand("~/.ssh/id_ed25519")
  if (file.exists(llave_ssh)) {
    Sys.chmod(llave_ssh, "600")
  }

  remotos <- tryCatch(
    system2("git", "remote", stdout = TRUE),
    error = function(e) character(0)
  )

  if ("origin" %in% remotos) {
    system2("git", c("remote", "set-url", "origin", repo_github))
  } else {
    system2("git", c("remote", "add", "origin", repo_github))
  }

  download.file(gitignore_url, ".gitignore", quiet = TRUE)
  message("Gitignore descargado")

  system2("git", "status")
  system2("git", c("add", "."))

  estado_commit <- system2("git", c("commit", "-m", "Primer commit"))
  if (estado_commit != 0) {
    message(
      "No se genero commit nuevo. ",
      "Puede que no existan cambios."
    )
  }

  system2("git", c("branch", "-M", "main"))
  estado_push <- system2("git", c("push", "-u", "origin", "main", "--force"))
  message("Proyecto subido correctamente")

  invisible(estado_push)
}
