% Generated from roxygen comments for offline documentation. Do not edit by hand.
\name{PushInicial}
\alias{PushInicial}
\title{Realizar el primer push forzado de un proyecto a GitHub}
\usage{
PushInicial(...)
}
\arguments{
  \item{repo_github}{URL del repositorio remoto de GitHub.}
}
\value{Invisiblemente, el codigo de estado retornado por `git push`.}
\description{
Reinicia la configuracion Git local del proyecto actual, descarga el
`.gitignore` corporativo, crea el commit inicial y lo envia a la rama
`main` del repositorio remoto indicado.
}
