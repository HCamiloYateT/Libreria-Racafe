# Validación de archivos y notificaciones — 0.5.0

Validado el 28 de octubre de 2026. Cambios limitados a racafeForms; sin modificaciones
en aplicaciones consumidoras, endpoints, bases de datos ni servicios externos.

## Implementación

- `racafeInputFile()` conserva el binding y protocolo nativos de `shiny::fileInput()`.
  Añade presentación compacta/normal, nombres y tamaños, ayuda, deshabilitado y errores.
- `racafeValidateInputFile()` valida en el servidor estructura, extensión/MIME declarado,
  tamaño real y declarado, vacío y lectura del archivo temporal. El consumidor debe
  validar el contenido con su lector y aplicar sus permisos antes de actuar.
- `racafeNotify()` y `racafeRemoveNotify()` reutilizan las notificaciones nativas, sus
  temporizadores y actualizaciones. El contenedor propio permanece dentro del panel
  nativo para conservar búsquedas por ID, eliminación y actualización de Shiny.
- CSS y manejadores compartidos se limitan a clases `rf-`; no hay dependencias nuevas.
  Compartido es opcional para estos componentes; su cargador sigue intacto.
- Alert y Confirm mantienen su implementación. La demo solo ejecuta acciones con
  `isTRUE()`: cancelar, cerrar o Escape nunca ejecutan la acción. La configuración
  existente puede impedir cerrar Confirm con Escape; en ese caso permanece abierto.

## Resultados

| Validación | Resultado |
| --- | --- |
| testthat completo de racafeForms | 503 comprobaciones, sin fallos |
| Navegador: archivos/avisos en Shiny | 43 comprobaciones |
| Navegador: archivos/avisos en bs4Dash | 44 comprobaciones |
| Navegador: bs4Dash con Compartido | 44 comprobaciones |
| Regresión de componentes corporativos | 74 comprobaciones |
| Regresión de bindings de texto | 42 comprobaciones |
| R CMD check --no-manual | Status: OK |
| Auditoría de documentación | Ayuda para las 50 funciones públicas |

Las pruebas de navegador cargan un XLSX y PDF mínimos reales y CSV múltiples, comprueban
`input$id` y la lectura del archivo temporal en dos módulos independientes. Cubren
extensión, límite, vacío, error de lectura simulado, nombres Unicode y texto escapado;
comprueban avisos por tipo, actualización por ID, segundos, persistencia, cierre,
namespaces, máximo visible, controles editables, navbar, sidebar móvil, modales y
confirmaciones. Una notificación ajena conserva su estilo, ubicación y permanencia.
Se ejecutan sin solicitudes externas ni errores JavaScript. Las pruebas no envían correos.

Entorno: R 4.5.0, Shiny 1.10.0, bs4Dash 2.3.5, htmltools 0.5.8.1,
Playwright 1.62.1 y Chromium 154. Se verificó escritorio y viewport móvil en Chromium;
no constituye certificación de todos los navegadores o dispositivos físicos.

## Reproducción

Desde la raíz del repositorio, con las dependencias de desarrollo instaladas:

```sh
Rscript -e 'roxygen2::roxygenise("racafeForms"); testthat::test_local("racafeForms")'
Rscript scripts/check_documentation.R
R CMD build racafeForms
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual racafeForms_0.5.0.tar.gz
R CMD INSTALL racafeForms
```

Inicie la demo en una terminal (bs4Dash por defecto):

```sh
Rscript -e 'shiny::runApp("racafeForms/inst/examples/files-notifications.R", port=3900)'
```

Con Playwright disponible según `tests/browser/package.json`, ejecute en otra terminal:

```sh
RACAFE_TEST_URL=http://127.0.0.1:3900 node racafeForms/tests/browser/test-files-notifications.cjs
```

Puede indicar `CHROMIUM_PATH` y `CI=1` para un navegador instalado en un contenedor.
`NODE_PATH` debe apuntar a los módulos si Playwright está instalado fuera del proyecto.
Repita la demo con `RACAFE_FILES_BS4DASH=0` para Shiny simple y con
`RACAFE_SHARED_PATH=/ruta/a/Compartido` para la integración corporativa opcional.
Las regresiones usan `corporate-components.R` / `test-corporate-components.cjs` y
`text-input-bindings.R` / `test-text-input-bindings.cjs`, con su respectiva URL.
