# Componentes visuales: racafeForms 0.4.0 / Compartido 1.1.0

Validación realizada el 28 de octubre de 2026. Se amplían los componentes existentes;
la única función pública nueva es racafeStatusBadge. La revisión de los 20 componentes
solicitados se detalla en [component-matrix.md](component-matrix.md).

## Arquitectura y alcance

- Compartido mantiene el CSS, los tokens corporativos y los scripts. La copia validada es
  `4f15ef36f2b31870c19823b1c66f1f30ce276d94`, con ASSETS_VERSION 1.1.0.
- racafeForms construye etiquetas R; no añade paletas, tamaños de modal ni handlers JS alternativos.
  Las reglas nuevas se limitan a clases rf-. Los tokens semánticos de Compartido sustituyen
  los literales de tres badges de entorno existentes sin cambiar sus colores visibles.
- Los estilos base previos del paquete se conservan para consumidores sin Compartido.
  Al cargar 1.1.0, la identidad y las densidades activas vienen de sus variables compartidas;
  se comprobó que cambiar el rojo corporativo se propaga a badges y controles.
- racafeModulos se consultó solo como referencia: commit
  `00fb6e51b8c529a11be19a11f20c54ada63db3b0`, TablaReactable2 y sus opciones de columnas/celdas.
  No se copió ni modificó ese módulo. La demo usa reactable directamente para mostrar el badge genérico.
- No se modificaron Firma Digital, FastAPI, MySQL, OEAPP ni los recursos de racafeMail.

## Cambios

Panel y tarjeta admiten header adicional, conservando title/cuerpo/footer. La tarjeta e indicador
añaden padding compact/normal/comfortable, con normal como valor anterior. Las tarjetas se alinean
por contenido en las cuadrículas: una tarjeta corta no hereda la altura de otra larga. Metric
conserva value/label/detail/icon y las cuadrículas de dos, tres o cuatro columnas, adaptables al móvil.

StatusBadge acepta cinco variantes semánticas y tres tamaños; escapa el valor como texto y no
incluye estados de negocio, eventos ni anuncios aria-live por celda. Los colores predeterminados
cumplen contraste mínimo 4,5:1; personalizar la paleta requiere volver a comprobarlo.

Los campos de texto, lista y rango usan las mismas dimensiones, fuente y padding dentro/fuera
de racafeForm. Las barras conservan grupos izquierdo/central/derecho y envuelven sus botones sin
CSS de aplicación. ButtonDownload ya admitía icon=NULL: se documentó y se verificó tanto esa ruta
como un único icono y la descarga real. Modales y portapapeles reutilizan sus constructores,
clases de tamaño y scripts sin registrar JavaScript desde cada módulo.

## Validación ejecutada

| Comprobación | Resultado |
|---|---|
| testthat | 385 correctas; 0 fallos, advertencias u omisiones |
| Compatibilidad API | Orden/nombres de argumentos de las 45 funciones de 0.3.0 conservados |
| R CMD INSTALL / build | Correctos |
| R CMD check --no-manual | Status: OK con _R_CHECK_FORCE_SUGGESTS_=false; devtools no está instalado |
| Ayuda RStudio | 46 funciones públicas y tema del paquete; verificador de los ocho paquetes OK |
| Chromium con Shiny / Bootstrap 3 | 74 comprobaciones correctas |
| Chromium con bs4Dash / Bootstrap 4 | 74 comprobaciones correctas |
| Demo heredada sin Compartido | Carga, binding numérico, actualización y confirmación nativa correctos |
| Recursos | Sin URLs duplicadas de hojas/scripts, sin peticiones externas ni errores JS/servidor |
| Sintaxis/formato | R y JS válidos; líneas R de producción y ejemplo <=120 caracteres; git diff --check correcto |

Las pruebas de navegador incluyen: badges dentro de dos reactable con namespace, contraste WCAG,
cambio de token corporativo, campos idénticos en tres densidades y fuera de formulario,
altura distinta de tarjetas vecinas, densidades ordenadas, cuadrículas 2/3/4 de escritorio y móvil,
barra/página sin desborde horizontal, descarga real con/sin icono, modal dinámico con badge y siete
anchos corporativos exactos. También se conservan las pruebas de copia real de texto/PNG,
copia desde R, UI dinámica, denegación de permisos, promesas pendientes, reset por binding y
módulos simultáneos. Los scripts no se duplican al cargar contenido dinámico.

Entorno: R 4.5.0, shiny 1.10.0, shinyWidgets 0.9.1, htmltools 0.5.8.1,
bs4Dash 2.3.5, reactable 0.4.5, Playwright 1.62.1 y Chromium 154. Se comprobó escritorio
1440x1100 y móvil 390x844. No se afirma validación en otros navegadores ni dispositivos físicos.
Los permisos del portapapeles se probaron en localhost; su uso remoto depende de HTTPS,
permisos y soporte del navegador. La demo suprime las fuentes/CDN opcionales de bs4Dash.

## Instalación para revisión y reproducción

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeForms",
                        ref = "codex/forms-visual-components", dependencies = NA, upgrade = "never")
# reactable y bs4Dash son Suggests; instalar para ejecutar ambos modos de la demo.
install.packages(c("reactable", "bs4Dash"))
```

Prepare el clon de Compartido en el commit fijado y use esa ruta local. No se descarga contenido
en cada render ni se sirve la raíz del repositorio. Para liberar, registre también el SHA completo
del paquete aprobado, sustituyendo las ramas por commits reproducibles.

```sh
git -C /ruta/Compartido checkout --detach 4f15ef36f2b31870c19823b1c66f1f30ce276d94
export RACAFE_SHARED_PATH=/ruta/Compartido
# Desde la raíz del monorepo:
Rscript -e 'testthat::test_local("racafeForms", stop_on_failure=TRUE)'
Rscript scripts/check_documentation.R
R CMD build racafeForms
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual racafeForms_0.4.0.tar.gz
Rscript -e 'shiny::runApp("racafeForms/inst/examples/corporate-components.R", port=3880)'
# Para bs4Dash, iniciar otra instancia con RACAFE_DEMO_BS4DASH=1 y otro puerto.
```

En tests/browser, ejecutar `npm ci` e instalar Chromium de Playwright o indicar CHROMIUM_PATH.
Ejecutar `RACAFE_TEST_URL=http://127.0.0.1:3880 CI=1 CHROMIUM_PATH=/usr/bin/chromium npm test`.
Repetir contra la instancia bs4Dash. La demo es autocontenida y usa datos ficticios.

## Compatibilidad y adopción

| Consumidor | Recursos | Comportamiento |
|---|---|---|
| Controles anteriores sin Compartido | Dependencia base | Bindings y estilos anteriores conservados |
| API 0.3.0 + Compartido 1.0.0 | Soportada por el cargador | Continúa la funcionalidad previa; sin las ampliaciones visuales 1.1.0 |
| racafeForms 0.4.0 con badges/densidades/campos uniformes | Compartido 1.1.0 | Cargar explícitamente racafeSharedAssets una vez en la UI |
| Módulos | ns en constructores; ID local en actualizadores | Namespace una sola vez; badge no requiere ID |
| reactable y bs4Dash | Dependencias sugeridas | No se añaden Imports ni dependencia de racafeModulos |

Para adoptar, actualizar juntos los recursos locales y el paquete y cambiar el pin explícito
de `version="1.0.0"` a `"1.1.0"` donde se quiera usar la ampliación. Las referencias históricas
0.3.0 de otros documentos se conservan como historial. No hay migración de datos ni servicios.
Para rollback, restaurar los commits previos y los pins de ambas copias; retirar las llamadas a
StatusBadge y los argumentos nuevos si se vuelve a racafeForms 0.3.0. No combinar estilos nuevos
copiados a la app con recursos antiguos.
