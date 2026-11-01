# Componentes UI corporativos — borrador sin publicar

Solo se modifican Libreria-Racafe y racafeModulos. No se modifica CRM Nacional,
PlantillaBs4Dash o Compartido. No se hacen commits, pushes, tags ni publicaciones;
las versiones de los paquetes se conservan.

## Arquitectura y reutilización

- racafeCollapsible usa details/summary nativos. El único JS nuevo es un InputBinding
  Shiny: registra input$id lógico, sincroniza aria-expanded y recibe actualizaciones.
  Emite shown/hidden para que Shiny reactive outputs suspendidos y redimensione gráficos.
- racafeUpdateCollapsible acepta ID local o ya namespaced dentro de la misma sesión.
  Delega a sendInputMessage, que aplica el namespace de la sesión exactamente una vez.
- racafeModalFooter reutiliza racafeForms::racafeModalFooter y sus botones nativos.
  El CSS propio se limita a .rsh-modal-footer; no cambia modales/bootstrap ajenos.
- TarjetaUI/Tarjeta pertenecen a racafeModulos. Se revisó CajaModal y no había Tarjeta
  en main. Se mantiene su patrón UI/moduleServer/reactivos, sin modificar CajaModal.
  El botón se renderiza aparte de los valores, sin listeners JS nuevos.
- Todos los estilos nuevos están en recursos de los paquetes. No se necesitan
  Compartido, estilos pegados por módulo, CDN, APIs, datos corporativos ni librerías JS nuevas.

## Integración orientativa con CRM Nacional

Reemplace únicamente la construcción de UI cuando decida integrar los componentes:

```r
# UI de un módulo padre; sus tablas y formularios existentes se conservan:
racafeShiny::racafeCollapsible(ns("detalle_pedidos"), "Detalle de pedidos, lotes y fechas",
  open = TRUE, shiny::tableOutput(ns("pedidos")))
racafeShiny::racafeCollapsible(ns("informacion_comercial"), "Información comercial (editable)",
  open = FALSE, formulario_existente_ui)
racafeModulos::TarjetaUI(ns("pedidos_kpi"))

# Servidor del mismo módulo:
pedidos <- racafeModulos::Tarjeta("pedidos_kpi", "Pedidos", valor = shiny::reactive(nrow(datos())),
  descripcion = "Creación: 20/10/2026", boton = TRUE)
shiny::observeEvent(pedidos$accion(), {
  shiny::showModal(shiny::modalDialog(title = "Detalle de pedidos", contenido_existente_ui,
    footer = racafeShiny::racafeModalFooter(acciones =
      racafeForms::racafeButtonSave("guardar", "Guardar", ns = session$ns))))
})
racafeShiny::racafeUpdateCollapsible(session, "informacion_comercial", open = TRUE)
```

Los nombres formulario_existente_ui, contenido_existente_ui y datos() representan objetos
propios de la aplicación: no se importan desde CRM. Registre los servidores hijos una
sola vez en el padre, fuera del observer que abre el modal. Mantenga validaciones,
permisos, confirmaciones y persistencia en la aplicación; la tarjeta solo expone eventos.
Un cierre nativo no guarda. Para Cancelar/Guardar con eventos, use cerrar FALSE y
observers separados que llamen removeModal. La demo implementa el ciclo completo.

## Validación y límites

R 4.5.0, Shiny 1.10.0, bs4Dash 2.3.5, htmltools 0.5.8.1 y Chromium 154.

- 23 comprobaciones nuevas de racafeShiny, sin fallos ni avisos.
- 22 comprobaciones de Tarjeta con testServer: valor/título reactivos, botón estable,
  contador de acciones, formatos, NA, datos escapados y ausencia de eventos sin botón.
- 38 comprobaciones de navegador por entorno: bs4Dash y Shiny simple. Se validan
  seis tarjetas/3 columnas, teclado, triángulo rojo y orientación, namespaces anidados,
  updates, inputs lógicos, outputs/gráficos suspendidos, reactividad, modal con módulo,
  acciones Cancelar/Guardar, reaperturas sin listeners duplicados, footer ajeno,
  móvil y deduplicación de recursos. No hay solicitudes externas ni errores JS.
- Se comparó la suite completa de racafeShiny contra una copia de HEAD sin cambios:
  ambas se detienen con los mismos 28 fallos y 4 avisos preexistentes (277 comprobaciones
  aprobadas antes de detenerse). Incluyen tests antiguos de helpers ausentes, patrones
  de formato/HTML y clases de modal. No se alteraron esas APIs para ocultar sus fallos.
- racafeShiny instalado localmente y demo ejecutada en navegador con sus APIs reales.
  racafeModulos no pudo instalarse completo porque falta su dependencia heredada racafe,
  que requiere a su vez odbc, fable y otros paquetes no disponibles aquí. Sus pruebas
  nuevas cargan Tarjeta.R directamente. Para el navegador se usa ese mismo módulo y
  la demo con rutas locales de recursos mediante un arnés temporal, no distribuido.
  No se certifica el chequeo completo de racafeModulos ni de los paquetes legacy.
- Se analizó la sintaxis y se generaron los dos paquetes con R CMD build. La auditoría
  de documentación de Libreria-Racafe reconoce las 47 funciones públicas de racafeShiny.

## Compatibilidad y decisiones

No se cambian argumentos de funciones existentes ni versiones. Tarjeta es una API nueva;
CajaModal permanece intacto con su comportamiento de modal. Se conservan las dependencias
runtime de ambos paquetes; racafeForms ya era sugerido por racafeShiny. En racafeModulos
se declaran racafeForms para la demo y testthat/edition 3 para las pruebas.

racafeShiny::racafeModalFooter y racafeForms::racafeModalFooter son homónimos con firmas
distintas. Use namespaces explícitos para evitar que el orden de library() cambie la
función que recibe una llamada sin calificar. Este adaptador no sustituye globalmente
el helper anterior ni cambia los footer predeterminados de otros modales.

Los HTML details y summary son nativos de navegadores actuales; requieren JavaScript
solo para sincronizar estado con Shiny. Los datos de título/valor/descripción de Tarjeta
son texto; si necesita UI arbitraria, colóquela dentro del colapsable o del modal.

## Archivos completos modificados o añadidos

Libreria-Racafe:

- racafeShiny/R/13_ui_components.R
- racafeShiny/inst/ui/components.css y components.js
- racafeShiny/NAMESPACE
- racafeShiny/man/racafeCollapsible.Rd, racafeUpdateCollapsible.Rd y racafeModalFooter.Rd
- racafeShiny/tests/testthat/test-ui-components.R
- racafeShiny/README.md, NEWS.md y docs/ui-components.md

racafeModulos:

- R/Tarjeta.R y inst/ui/tarjeta.css
- NAMESPACE y DESCRIPTION (solo dependencias sugeridas/herramientas de pruebas)
- man/Tarjeta.Rd y TarjetaUI.Rd
- inst/examples/componentes-ui.R
- tests/testthat.R, tests/testthat/test-tarjeta.R
- tests/browser/test-componentes-ui.cjs y package.json
- README.md, NEWS.md y docs/ui-components.md

## Reproducción

Con las dependencias instaladas, instale las copias locales (no las versiones remotas,
que todavía no contienen este borrador):

```sh
# Desde Libreria-Racafe:
R CMD INSTALL racafeShiny
Rscript -e 'testthat::test_local("racafeShiny", filter="ui-components", stop_on_failure=TRUE)'
# Desde racafeModulos:
R CMD INSTALL .
Rscript -e 'testthat::test_local(filter="tarjeta", stop_on_failure=TRUE)'
```

Demo ejecutable:

```r
shiny::runApp(system.file("examples", "componentes-ui.R", package = "racafeModulos"), port = 3940)
# Repita con Sys.setenv(RACAFE_UI_BS4DASH = "0") para Shiny simple.
```

En otra terminal, desde racafeModulos y con Playwright disponible:

```sh
RACAFE_TEST_URL=http://127.0.0.1:3940 node tests/browser/test-componentes-ui.cjs
```

El test admite CHROMIUM_PATH, NODE_PATH y CI=1 para contenedores. No usa correos, APIs
ni bases de datos. Todo queda como cambios locales para revisión del usuario.
