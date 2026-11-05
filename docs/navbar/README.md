# Listas, detalle y loaders corporativos

## Auditoría y compatibilidad

| Base existente | Resultado | Compatibilidad |
| --- | --- | --- |
| `racafeStatusBadge`, `racafeFormGroup` | `racafeDataBadge`, `racafeDetailSection` delegan en ellos | Sin cambiar variantes ni estilos anteriores |
| `racafeModal`, `racafeModalFooter`, `racafeStack` | `racafeModalDetail` compone esas funciones | Footer personalizado se conserva; tamaño inicial corporativo |
| `checkboxGroupInput`, infraestructura de inputs | `racafeChecklist` usa binding nativo | Claves nombradas o posiciones; sin persistencia |
| `gt_minimal_style` | Añade `compact = TRUE` y opciones de tabla | Por defecto conserva el estilo previo |
| Recursos `racafePreloader` | `racafeOverlayLoader` reutiliza spinner y dependencia | No cambia los loaders de aplicación existentes |
| Dependencia UI `rsh-*` | ListItem, ListGroup y CompactTable | CSS acotado; texto escapado; selección delegada |
| `MenuHeaderUI` / `MenuHeaderServer` | Amplía el módulo existente | Mantiene argumentos anteriores, ancho 280px y selección/retorno |
| `DemoMenuHeader()` | `DemoMenuHeader(corporate = TRUE)` | FALSE conserva la demostración anterior |

`MenuHeaderUI()` devuelve un `li`, directamente utilizable en `bs4DashNavbar(rightUi = tagList(...))`.
`keep_open_on_select = NULL` conserva los clics internos anteriores; TRUE añade conservación durante modales;
FALSE solicita el cierre al seleccionar. `direct = TRUE` permite abrir el único registro de un data.frame.
No se necesitan IDs globales ni estilos dentro de `rightUi`.

## Instalación coordinada

Instalar primero las dos subcarpetas de esta librería y después el módulo. Mientras se revisan los PR,
utilizar `ref = "codex/navbar-components"`; tras integrarlos puede omitirse `ref`.

```r
install.packages("remotes")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeShiny", ref = "codex/navbar-components")
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeForms", ref = "codex/navbar-components")
remotes::install_github("HCamiloYateT/racafeModulos", ref = "codex/navbar-components")
racafeModulos::DemoMenuHeader(corporate = TRUE)
racafeForms::racafeFormsGallery()
```

Versiones: racafeShiny 1.3.0, racafeForms 0.7.0, racafeModulos 0.2.0. Nuevos imports explícitos de
racafeShiny: jsonlite y tidyselect. La galería utiliza later (Suggests en racafeModulos).
No se añaden librerías JavaScript externas ni conexiones a bases de datos.

## Uso de listas y selección

```r
ns <- shiny::NS("demo")
racafeShiny::racafeListGroup(
  racafeShiny::racafeListItem("Café & compañía", metadata = c(NIT = "900123456"),
    badges = racafeForms::racafeDataBadge("Estado", "Pendiente"),
    id = "selected", value = list(key = "c1"), ns = ns))
# En moduleServer("demo", ...), input$selected contiene list(key = "c1").
```

Dentro de MenuHeaderServer se puede omitir `id`: el módulo asigna su canal de selección.
El texto de títulos, metadatos y tablas se escapa. `html_columns` de CompactTable es una autorización
explícita para contenido HTML de confianza; no debe utilizarse con contenido sin sanear de usuarios.
El backend gt conserva la clase gt_tbl y permite operaciones gt posteriores. Al renderizar prefija
las referencias de encabezados por tabla para evitar IDs repetidos entre módulos.

## Loader y consultas

```r
# UI dentro de un módulo
racafeShiny::racafeOverlayLoader("panel", shiny::textOutput(ns("result")), ns = ns, triggers = "consultar")
# Server: la operación consumidora controla el final, incluso si falla.
shiny::observeEvent(input$consultar, {
  racafeShiny::racafeUpdateOverlayLoader(session, "panel", TRUE)
  on.exit(racafeShiny::racafeUpdateOverlayLoader(session, "panel", FALSE), add = TRUE)
  # Consulta y actualización de outputs de la aplicación.
})
```

`triggers` muestra el overlay en el navegador antes de que el servidor ejecute una tarea bloqueante;
acepta botones y confirmaciones lógicas. En promesas/callbacks cierre en ambas ramas o finally.
El idle de otros módulos no lo oculta. Un error de output dentro del panel y la desconexión sí lo limpian.
El overlay es visual: no convierte consultas síncronas en asíncronas; para mantener el servidor disponible
se necesita una estrategia asíncrona en la aplicación consumidora.

## Migración de los seis adjuntos

Los scripts originales contienen lógica comercial y SQL. Se entregan como copias y parches fuera del
paquete; el repositorio de componentes conserva solo el generador y los verificadores.

| Adjunto | Sustitución visual |
| --- | --- |
| `00_header.R` | Badges, lista seleccionable, tabla compacta y modal de detalle; elimina JS específico |
| `01_tabla_indicadores.R` | Overlay local inicial, secciones y estilo gt compacto |
| `02_notificaciones_tareas.R` | Checklist de claves estables, fechas, listas y detalle |
| `03_form_persona.R` | Sin cambios: ya utiliza componentes corporativos |
| `04_migrar_cartera.R` | Overlay local activado por confirmación, cierre garantizado |
| `header.R` | MenuHeaderUI directo en navbar, tamaño y retención configurables |

```sh
python3 docs/navbar/prepare_migration.py /ruta/originales /ruta/migrados
Rscript docs/navbar/verify_migration.R /ruta/originales /ruta/migrados
Rscript docs/navbar/check_migration_ui.R /ruta/migrados
```

El verificador conserva comentarios, literales SQL, llamadas DBI/racafeBD y cuerpos de funciones comerciales
protegidas; comprueba además las 16 combinaciones de estados de un checklist de cuatro actividades.
La prueba UI evalúa únicamente definiciones y construye siete interfaces con datos ficticios. No ejecuta
servidores del CRM, transacciones ni consultas. La aplicación real debe revisarse en staging con sus datos
y credenciales antes de sustituir archivos. Los recursos GIF existentes siguen siendo responsabilidad del CRM.

## Validación reproducible

```r
testthat::test_local("racafeForms")
testthat::test_local("racafeShiny", filter = "navbar|loaders|ui-components")
# Desde el otro checkout:
testthat::test_local("/ruta/racafeModulos")
```

Ejecutar cada paquete en un proceso R separado. La prueba Playwright está en
`racafeModulos/tests/browser/test-navbar-components.cjs`; iniciar la galería y establecer
`RACAFE_NAVBAR_URL`, `CHROMIUM_PATH` y NODE_PATH para Playwright. La regresión de la galería de formularios
está en `racafeForms/tests/browser/test-gallery.cjs` con `RACAFE_TEST_URL`.

Resultados de esta implementación y limitaciones: [VALIDATION.md](VALIDATION.md).
