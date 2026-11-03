# Galería interactiva y ampliación de componentes

## Ejecutar e instalar

Esta revisión no incrementa la versión del paquete. Desde Libreria-Racafe:

```r
remotes::install_local("racafeForms", dependencies = NA, upgrade = "never")
if (!requireNamespace("bs4Dash", quietly = TRUE)) install.packages("bs4Dash")
# Reiniciar R y recargar el navegador sin caché después de instalar.
library(racafeForms)
racafeFormsGallery()
```

La función devuelve una aplicación creada con `system.file()` y `shinyAppDir()`, igual que
la referencia de shinyWidgets. También puede iniciar `inst/examples/racafeFormsGallery/`
con `shiny::runApp()`. No ejecute una segunda aplicación desde un observer de otra aplicación.
`bs4Dash` permanece en Suggests: los controles no lo requieren para funcionar fuera del dashboard.

La galería funciona localmente con los recursos incluidos. No usa CRM, bases de datos,
servicios, fuentes remotas ni información empresarial real. La ayuda HTML se genera desde
la documentación instalada. El navegador puede impedir copiar texto o imágenes por permisos;
se comunica el resultado real, sin informar éxito anticipadamente. Use localhost o HTTPS.

## Inventario, configuración y código

El menú agrupa las 58 funciones públicas, incluidos actualizadores, validadores, archivos,
portapapeles, notificaciones y recursos. Añade un formulario completo y una integración opcional con ListaDesplegableAgregable
de racafeModulos (60 entradas en total). La prueba de inventario
compara las exportaciones contra el catálogo: un componente nuevo exige una demostración.
Las funciones privadas no se muestran como componentes.

Cada ejemplo tiene namespace y servidor propios, registrados una sola vez. Los parámetros
se conservan por componente; el tamaño puede ser compact, normal o comfortable. Configure y
pulse **Aplicar configuración** para reconstruir únicamente el ejemplo activo. Reconstruir
la UI restablece sus controles al valor inicial configurado. Los parámetros solo se muestran
para los controles que los admiten. Los valores reales/tipos enviados por Shiny se muestran
junto a la demostración.

Los ejemplos de numéricos permiten cambiar formato, límites, valor, decimales, separadores,
readonly y disabled; el estándar predeterminado continúa siendo `1,234.56`. Los selectores
permiten selección múltiple, búsqueda, vacío, género, tamaño, deduplicación y actualización.
El formulario completo incluye dos grupos, campos obligatorios, correo condicional,
habilitación mediante switch, confirmación, mensaje de resultado y descarga validada.

El código copiable y la previsualización provienen de la misma definición; cada ejemplo
incluye UI, moduleServer y shinyApp. Ejecute el bloque completo en una sesión nueva. Las
acciones de servidor y permisos de portapapeles conservan las restricciones del navegador.
La demostración de la propia función Gallery es la aplicación activa; su tarjeta explica
cómo abrir otra instancia desde una sesión independiente.

## Cambios y compatibilidad

### Superficies y densidad

`racafeForm(..., variant = NULL)` conserva exactamente la selección anterior de `bordered`
y `unstyled`. Si se especifica, `variant` tiene prioridad sobre ambos:

| Variante explícita | Apariencia |
| --- | --- |
| plain | Transparente, sin marco ni relleno adicional |
| soft | Fondo gris corporativo, borde sutil y radio existente |
| bordered | Fondo blanco, borde visible y radio existente |

`racafeFormGroup(..., variant = NULL)` mantiene el aspecto anterior; acepta las mismas
variantes. El divisor sigue separando grupos consecutivos sin duplicar el borde del footer;
`divider = FALSE` lo desactiva. Dentro de un panel use plain; no añada otra tarjeta ni hr
entre grupos. Los títulos principales pertenecen a la navegación. Encabezados de sección,
labels, radios de borde y densidades reutilizan los estilos existentes. Compact mantiene
36px, normal 36px con más separación y comfortable 40px. Las áreas de texto conservan filas.

### Selectores

`racafeInputSelect(..., unique_choices = FALSE)` conserva los duplicados anteriores por
defecto. TRUE elimina valores repetidos conservando la primera etiqueta y los grupos.
Solo se permite una opción con valor vacío, aun cuando venga repetida en choices. No se
transforman valores válidos ni se convierten vacíos en selecciones reales. `empty`,
`empty_label`, `gender`, `search` y `size` cubren los antiguos helpers picker.
`racafeUpdateSelect()` conserva selecciones válidas si no se indica selected y elimina
vacíos duplicados del catálogo nuevo. No deduplica otros valores en el actualizador:
prepare el catálogo sin duplicados si también lo necesita en actualizaciones.

### Habilitación y mensajes

```r
racafeForms::racafeUpdateEnabled(session, "cuenta_padre", enabled = FALSE)
racafeForms::racafeEnable("cuenta_padre", session)
racafeForms::racafeDisable("cuenta_padre", session)
racafeForms::racafeToggle("cuenta_padre", condition = TRUE, session = session)
```

Los tres helpers anteriores conservan los argumentos posicionales y añaden session al
final. Ahora delegan en el mismo mecanismo del paquete y no requieren useShinyjs().
La dependencia obligatoria shinyjs se elimina; las aplicaciones que la usan directamente
deben seguir declarándola por su cuenta. Se actualizan tanto el control nativo como el botón
del picker y los estados de botones, sin cambiar los valores ni generar change artificiales.
Un estado enviado antes de renderUI se conserva para ese ID hasta otro estado o recarga de
página. Un ID reutilizado recibe el último estado conocido: envíe TRUE para revertirlo.
Dentro de moduleServer pase IDs locales; no duplique el namespace. Los estados visuales no
sustituyen permisos ni reglas de negocio en el servidor.

`racafeFormMessage()` reutiliza info/success/warning/error/muted/loading/empty y añade
`size` e `icon`, ambos opcionales. Los tamaños nuevos solo cambian la presentación;
no se inserta HTML desde mensajes de texto. Se puede devolver directamente desde renderUI.

La validación reutiliza racafeFormValidation, racafeValidateForm y racafeShowValidation,
sin API paralela. Consulte [la guía de validación y migración](form-standardization.md).
Los botones conservan sus variantes e iconos anteriores y comparten disabled/loading;
racafeUpdateButton muestra progreso y confirmación. La galería prueba esos estados.

### Escape y modales

Un único listener de captura en la dependencia del paquete concede Escape a SweetAlert
antes de Bootstrap. Cancela la confirmación activa y conserva el modal inferior; no se
registra por instancia o formulario. No añada listeners locales para resolver este caso.
Los cierres por botones conservan el comportamiento existente. La prueba repite tres ciclos
de apertura y cierre, con un modal de easy_close = TRUE.

## Identidad visual sin CSS global

Compartido aporta `Styles/tokens.css` como única fuente de variables y
`Styles/components.css` como entrada que importa tokens y componentes rf-. Su style.css
continúa incorporando esas variables y sus reglas generales. El rojo principal se fija
en #DA291C. La galería utiliza:

```r
racafeForms::racafeSharedAssets(
  system.file("gallery-assets", package = "racafeForms"), global_css = FALSE
)
```

El valor predeterminado `global_css = TRUE` conserva el loader anterior. FALSE requiere
components.css y tokens.css; una copia antigua informa que debe actualizarse. No mezcle
modos ni versiones de Compartido en una misma página: las dependencias se deduplican.

`inst/gallery-assets` es una instantánea de distribución de Compartido, no otra
implementación. Mantenga sus archivos idénticos a la fuente; no edite CSS/JS directamente
en esa carpeta. Se incluyen solo tokens, entrada de componentes, CSS rf-, scripts existentes,
logos y ASSETS_VERSION. No se distribuye el CSS general de aplicaciones. README indica
la procedencia y cómo sincronizarlos. No se duplica lógica de reset, portapapeles o tarjetas
con nuevos scripts en la galería.

## Referencia IndFormularioUI

No se encontró la implementación de IndFormularioUI ni ind_picker/crm_picker en las copias
de los repositorios de referencia. La [propuesta de adaptación](ind-formulario-proposal.md)
explica la sustitución sin inventar IDs ni alterar negocio. Requiere revisar el código real
antes de aplicar cambios. No se han editado aplicaciones productivas.

## Archivos y validación

Libreria-Racafe: R/gallery.R, R/00_shared_assets.R, R/00_utils.R, R/01_layout.R,
R/02_inputs.R, R/04_updates.R, inst/www/racafe-forms.css y .js, la aplicación en
inst/examples/racafeFormsGallery, inst/gallery-assets, tests/testthat/test-gallery.R,
tests/browser/test-gallery.cjs, DESCRIPTION, NAMESPACE, man, README, NEWS y estas guías.
Compartido: Styles/style.css, tokens.css, components.css y README. PlantillaBs4Dash:
ambos módulos demo, ambos global.R y README. racafeModulos se revisó sin cambios necesarios.

Para validar con dependencias de desarrollo instaladas:

```sh
Rscript -e 'testthat::test_local("racafeForms")'
R CMD build --no-build-vignettes racafeForms
_R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual racafeForms_0.5.0.tar.gz
Rscript -e 'shiny::runApp(racafeForms::racafeFormsGallery(), port=4002)'
RACAFE_TEST_URL=http://127.0.0.1:4002 node racafeForms/tests/browser/test-gallery.cjs
```

La prueba usa Playwright con Chromium; admite CHROMIUM_PATH, NODE_PATH y CI=1. Cubre las
categorías, ayuda, configuración, copia, valores, selectores, habilitación dinámica,
confirmaciones, reset, descarga, móvil y ausencia de recursos externos/errores JavaScript.
Las pruebas de negocio de las aplicaciones consumidoras siguen siendo necesarias.

Resultados de la revisión (3 de noviembre de 2026):

| Validación | Resultado |
| --- | --- |
| testthat completo | 819 comprobaciones; sin fallos, avisos ni omisiones |
| R CMD check, sin manual PDF | OK |
| Galería, 59 ejemplos | 197 comprobaciones en Chromium |
| Regresión de formularios estandarizados | 43 comprobaciones |
| Regresión de componentes corporativos | 74 comprobaciones |
| PlantillaBs4Dash, R / navegador | 56 / 60 comprobaciones |
| Ayuda instalada | 58 funciones públicas documentadas |
| Recursos incluidos | Idénticos a los archivos canónicos de Compartido |

Las comprobaciones de navegador no sustituyen pruebas en otros motores. No se ejecutó
persistencia empresarial ni se modificaron permisos o reglas de negocio productivas.

## Actualización de validación y listas agregables

La validación ya no abre SweetAlert ni muestra resúmenes para errores de campo, aunque
reciba mode = "alert" o "both". Solo se confirma una acción cuando sus campos son válidos.
Los selectores ofrecen uppercase para cambiar únicamente su presentación. La categoría
Agregables utiliza la revisión nueva de racafeModulos si está instalada; de lo contrario
explica qué instalar sin impedir el uso de la galería. Es una integración opcional (Suggests),
no una dependencia obligatoria de los controles ni una copia del módulo.
