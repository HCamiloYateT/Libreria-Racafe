# Modales y selectores en el controlbar

Revisión entregada mediante PR, sin publicar versiones de paquetes ni recursos.
No se modifica CRM Nacional. Las firmas públicas y los IDs de inputs se conservan.

## Dependencias y primera apertura del modal

```r
ui <- shiny::fluidPage(
  racafeForms::racafeFormsDependencies(),
  shiny::actionButton("abrir", "Abrir")
)
```

La dependencia base carga los estilos de controles y los tamaños corporativos desde el
inicio. Cualquier componente racafeForms inicial también adjunta esa dependencia; si solo
crea componentes RF con showModal(), incluya la dependencia explícitamente en la UI inicial.
La galería y DemoListaDesplegableAgregable la precargan de forma explícita.

racafeModal conserva el código nativo de apertura de Shiny y espera a que estén disponibles
los estilos RF antes de ejecutarlo. Así también protege aplicaciones antiguas que no
precargan la dependencia: el primer modal permanece oculto mientras llegan sus CSS y aparece
con el tamaño definitivo. Si los estilos fallan durante 15 segundos, informa el fallo de
carga; no muestra un modal con ancho provisional. La espera termina si se retira/sustituye
el modal, sin listeners acumulados. No se modifica globalmente shiny::showModal().

El ejemplo `inst/examples/modal-controlbar.R` permite verificar ambos caminos. Su parámetro
URL `?preload=0` omite todos los componentes RF iniciales; el test retrasa los CSS y comprueba
que ningún frame visible tenga una anchura excesiva.

## Footer corporativo

```r
shiny::showModal(racafeForms::racafeModal(
  "Contenido", title = "Confirmación", size = "aviso",
  footer = racafeForms::racafeModalFooter(
    shiny::modalButton("Cancelar"),
    shiny::actionButton(session$ns("confirmar"), "Confirmar")
  )
))
```

Las etiquetas Shiny de botones se normalizan antes de enviarse al navegador: rf-btn y tamaño
corporativo, Cancelar/Cerrar secundarios y Confirmar/Guardar/Aceptar principales rojos.
Las demás variantes RF explícitas se conservan. Se mantienen IDs, handlers, estados disabled,
datos de cierre y bindings nativos. No se agregan observadores de clic por botón.

El footer se mantiene horizontal y alineado a la derecha, también en móvil. Los grupos
left/center/right conservan el contenido y orden, pero se alinean a la derecha dentro de un
footer modal; racafeActionBar fuera del footer conserva su distribución anterior. Si los
botones no caben, pasan a otra fila horizontal sin desbordar. footer = NULL sigue omitiéndolo.
Un footer nativo pasado directamente a racafeModal se envuelve y normaliza igualmente.

## Fuente única de tamaños

Compartido/Styles/racafe-modal-sizes.css contiene las variables y reglas RF de tamaños.
application.css reutiliza esas variables con los mismos selectores y valores anteriores.
El constructor de Styles/style.css consolida tokens, componentes RF, tamaños y aplicación;
el resultado sigue siendo autocontenido y no introduce imports relativos en includeCSS().
components.css conserva su entrada independiente por URL e importa también los tamaños.

El mismo archivo se distribuye sin modificaciones en racafeForms/inst/www y en la instantánea
gallery-assets/Styles. Son copias de distribución de la fuente de Compartido, no tablas de
anchos mantenidas por separado. No edite esas copias: sincronice desde el origen y valide
su identidad byte a byte. Los límites existentes se conservan:

| Tamaño | Límite |
| --- | --- |
| ventana | min(96vw, 1200px) |
| subventana1 | min(94vw, 1050px) |
| subventana2 | min(92vw, 900px) |
| subventana3 | min(90vw, 760px) |
| subventana4 | min(88vw, 620px) |
| subventana5 | min(86vw, 500px) |
| aviso | min(84vw, 380px) |

## Selectores y bs4DashControlbar

Bootstrap sigue gestionando apertura/cierre del desplegable y Shiny conserva su binding.
La protección central identifica el origen real del clic mediante su ruta DOM, incluso si
Bootstrap retira/reconstruye el elemento pulsado. Para menús trasladados al body recuerda
qué controlbar contiene el selector. Detiene solo la subida de esos clics a window, donde
bs4Dash detecta clics externos; los handlers de document y el comportamiento de Bootstrap
se conservan. No cancela acciones por defecto ni elimina listeners de otros paquetes.

Los selectores simples cierran su menú normalmente y dejan abierto el controlbar; los
múltiples mantienen menú y panel abiertos al seleccionar. Botón de cierre, toggle y clic
externo continúan funcionando. Los handlers son delegados y se registran una sola vez.
Al retirar un selector dinámico se limpia su contenedor externo, sin menús huérfanos.
Los menús externos conservan clases, variables y presentación corporativa limitadas a rf-.

No requiere JavaScript en aplicaciones consumidoras. La galería incluye un controlbar con
selectores simples/múltiples, renderUI, reconstrucción y la opción de trasladar el menú al
body. DemoListaDesplegableAgregable incluye instancias adicionales simples/múltiples en su
controlbar y un modal con footer de botones nativos.

AGREGAR... primero y uppercase ya estaban implementados en main y se conservan: la acción
queda fuera de seleccion() y Todos; las actualizaciones conservan selecciones; uppercase
solo transforma presentación y puede desactivarse. No se cambian valores, catálogos ni
persistencia.

## Comprobaciones

- 58 verificaciones Chromium de primera apertura con CSS retrasado, tamaños, botones,
  namespace, footer móvil, controlbar, menús externos, renderUI y listeners no duplicados.
- 69 verificaciones de Compartido: sidebar idéntico a referencia, layout y controles.
- 880 comprobaciones R en racafeForms y R CMD check --no-manual con estado OK.
- 56 comprobaciones R en racafeModulos y 27 de construcción en Compartido.
- Regresiones de navegador: 200 de galería y 44 de listas agregables.
- racafeModulos mantiene 4 advertencias y 1 nota heredadas del check: LICENSE,
  código no ASCII, dependencias/documentación de otros módulos y NEWS. No se ocultan ni
  se corrigen modificando componentes ajenos a esta tarea.

```sh
Rscript -e 'shiny::runApp(racafeForms::racafeFormsGallery(), port=4023)'
Rscript -e 'shiny::runApp(racafeModulos::DemoListaDesplegableAgregable(), port=4024)'
Rscript -e 'shiny::runApp("racafeForms/inst/examples/modal-controlbar.R", port=4025)'
RACAFE_GALLERY_URL=http://127.0.0.1:4023 RACAFE_MODULE_URL=http://127.0.0.1:4024 \
RACAFE_MODAL_URL=http://127.0.0.1:4025 node racafeForms/tests/browser/test-modal-controlbar.cjs
```

Requiere Playwright/Chromium; admite NODE_PATH, CHROMIUM_PATH y CI=1. Se probó Shiny 1.10.0,
Bootstrap 3 y bs4Dash 2.3.5 (Bootstrap 4). Los checks no sustituyen pruebas de aplicaciones
productivas: no se ejecutó CRM ni se modificaron reglas de negocio.
