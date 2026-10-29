# Loaders corporativos — ampliación 1.2.0

## Cambios y compatibilidad

Se mantienen `racafePreloader`, `racafeLoaderUI` y los métodos `show()`, `update(html)`
y `hide()` de `racafeLoader`. Las doce operaciones comparten arquitectura waiter,
colores y animación sobria. Los argumentos previos conservan orden y significado,
excepto `logo = NULL`, que ahora muestra el logo corporativo. `show_logo = FALSE`,
añadido al final de las firmas, permite ocultarlo. Los tipos anteriores siguen válidos;
sus mensajes se amplían según la especificación.

El automático cede ante un waiter existente, incluido el preloader de bs4Dash.
La repetición de un objeto corporativo en el mismo destino no acumula overlays.
La desconexión limpia los overlays corporativos; no retira waiters ajenos, modales ni
notificaciones. Tras idle, los avisos `racafeNotify()` vuelven a quedar visibles.
Los IDs de módulos reciben namespace una sola vez. El contenedor debe existir.

No se añade `setProgress()`: un bucle síncrono bloqueante no puede transmitir avances
intermedios de forma fiable. Para tareas asíncronas se conserva control explícito de
mensajes y cierre; resolución y rechazo deben retirar el objeto. No se simulan porcentajes.

## Logo incluido

Recurso: `inst/loaders/logo2.png`, copiado para esta tarea por instrucción del usuario.
Origen: https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/img/logo2.png

Copia del repositorio Compartido, referencia `483c6909689d532df47f706dcbd8cc9cc7d3bd98`.
SHA-256: `6c895a56b886617e1aeac228b17eebb9ac441a3eef783f7c4c87b0611b2893f9`.
Se sirve desde la dependencia local de Shiny, nunca desde GitHub por defecto. Conserva
proporciones y un ancho de 70 px. Si una imagen personalizada falla se intenta la copia
local; si también falla se oculta la imagen sin bloquear el loader ni la operación.

## API, instalación y ejemplos

La API definitiva, mensajes por operación y ejemplos de instalación e integración están
en [README](../README.md#loaders-corporativos-120) y en la ayuda de RStudio de las tres
funciones. `waiter` continúa como dependencia obligatoria, sin nuevos imports.
Se declaran dependencias opcionales de demo: bs4Dash, racafeForms y readxl.

La demo conserva dos módulos y añade consulta, tabla, guardado/eliminación en memoria,
validación de archivos, importación CSV y XLSX, envío simulado, PDF real y descarga.
XLSX usa readxl; si falta, informa el requisito y cierra el loader. No hay correos reales,
servicios externos ni persistencia en bases de datos.

```sh
Rscript -e 'testthat::test_local("racafeShiny", filter="loaders", stop_on_failure=TRUE)'
Rscript scripts/check_documentation.R
R CMD build racafeShiny
R CMD check --no-manual racafeShiny_1.2.0.tar.gz
R CMD INSTALL racafeShiny
Rscript -e 'shiny::runApp("racafeShiny/inst/examples/loaders.R", port=3912)'
```

Con Playwright disponible, en otra terminal:

```sh
RACAFE_TEST_URL=http://127.0.0.1:3912 node racafeShiny/tests/browser/test-loaders.cjs
```

Repita con `RACAFE_LOADER_BS4DASH=1` y después añada `RACAFE_LOADER_PRELOADER=1`.
El test admite `CHROMIUM_PATH`, `NODE_PATH` y `CI=1` para contenedores.

## Validación y límites

Validación del 29 de octubre de 2026: R 4.5.0, Shiny 1.10.0, waiter 0.2.5.1,
bs4Dash 2.3.5 y Chromium 154.

- 161 comprobaciones unitarias del componente sin fallos ni avisos.
- 55 comprobaciones de navegador por modo: Shiny, bs4Dash y bs4Dash con preloader nativo.
- Se verifican logo local en las doce variantes, imagen alternativa fallida, ausencia de
  solicitudes externas, animación durante bloqueo de R, carga inicial sin duplicados,
  teclado, móvil, movimiento reducido, recursos únicos, repetición, namespaces,
  objetos locales, desconexión y preservación de waiters ajenos.
- Se comprueban avisos reales racafeNotify después del cierre, carga/importación CSV,
  validación de archivo y descarga de un PDF real generado localmente. XLSX prueba un
  archivo real y la ruta controlada de requisito opcional porque falta readxl.
- Ayuda disponible para las 44 funciones públicas; R CMD build genera el paquete.

**Límite del entorno:** faltan racafeGraph, gt y scales y CRAN rechazó su descarga.
Las pruebas nuevas cargan directamente `R/12_loaders.R`; el navegador usa la demo con
ese mismo código y recursos locales mediante un arnés temporal. No certifican la suite
completa. R CMD check se intentó y se detuvo con un error por esas dependencias
obligatorias ausentes; no se presenta como aprobado. El arnés no se distribuye.

No se modificaron racafeForms, Compartido, PlantillaBs4Dash, Firma Digital, endpoints,
MySQL ni otras aplicaciones consumidoras.
