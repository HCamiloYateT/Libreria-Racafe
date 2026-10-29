# Loaders corporativos — validación 1.1.0

Origen visual: `HCamiloYateT/PlantillaBs4Dash`, `APP/misc/ui/preloader.R`, y estilos
preloader de Compartido. Se conservaron los tres estados y se integraron sus recursos
al paquete; no se modificaron PlantillaBs4Dash, Compartido ni aplicaciones consumidoras.

## Decisiones

- Se utiliza waiter (ya empleado por la plantilla) y no se añade un segundo motor.
- `racafePreloader()` produce contenido para bs4Dash; `racafeLoaderUI()` activa en el
  navegador los loaders de inicio, busy/idle y botones configurados.
- Un host reservado del paquete evita que el cierre automático retire otros waiters.
- `racafeLoader()` encapsula Waiter y fija el dominio reactivo por método. Esto evita
  depender del dominio activo al invocar el objeto y preserva namespaces de módulos.
- CSS/JS se empaquetan sin CDN; el logo es opcional. La demo bs4Dash suprime sus
  dependencias externas predeterminadas ionicons/googlefonts; el paquete no cambia
  dependencias ni estilos de aplicaciones consumidoras.
- No se promete concurrencia ni progreso desde R bloqueado. La activación por clic
  ocurre antes de que llegue la acción al servidor. Para promesas se requiere cierre
  explícito al resolver y rechazar.

## Entorno y alcance

R 4.5.0, Shiny 1.10.0, waiter 0.2.5.1, bs4Dash 2.3.5 y Chromium 154.

- 44 comprobaciones unitarias del nuevo componente sin fallos ni avisos.
- 22 comprobaciones de navegador en Shiny y 22 en bs4Dash: carga inicial, activación por clic antes de
  respuesta, animación durante Sys.sleep(3), busy sin trigger, idle, error controlado,
  show/update/hide local, namespaces, aislamiento de otros waiters, repetición de ciclos,
  móvil, movimiento reducido, desconexión, recursos únicos y ausencia de solicitudes externas.
- Auditoría de documentación: 44 funciones públicas con ayuda de RStudio.
- R CMD build genera el paquete 1.1.0 correctamente.

**Límite de la validación:** faltan racafeGraph, gt y scales en este entorno y el
acceso a CRAN rechaza su descarga. Las pruebas nuevas se ejecutaron cargando el archivo
R del componente directamente; el navegador ejecutó la demo con ese mismo código y sus
recursos locales mediante un arnés temporal. No se ejecutó la suite completa ni se
certifica R CMD check del paquete completo. El arnés no forma parte del paquete.

## Reproducción con dependencias completas

```sh
Rscript -e 'testthat::test_local("racafeShiny", filter="loaders", stop_on_failure=TRUE)'
Rscript scripts/check_documentation.R
R CMD build racafeShiny
R CMD check --no-manual racafeShiny_1.1.0.tar.gz
R CMD INSTALL racafeShiny
Rscript -e 'shiny::runApp("racafeShiny/inst/examples/loaders.R", port=3910)'
```

En otra terminal, con Playwright instalado:

```sh
RACAFE_TEST_URL=http://127.0.0.1:3910 node racafeShiny/tests/browser/test-loaders.cjs
```

Repita iniciando la demo con `RACAFE_LOADER_BS4DASH=1`. En contenedores puede indicar
`CI=1`, `CHROMIUM_PATH` y `NODE_PATH` según la instalación del navegador y Playwright.
