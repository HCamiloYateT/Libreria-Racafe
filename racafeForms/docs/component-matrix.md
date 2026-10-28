# Inventario y decisiones previas a la implementación

Revisión: racafeForms 0.2.1 y Compartido main. Se revisaron R, CSS, JavaScript, ayuda, ejemplos y pruebas.
Compartido contiene Styles/style.css (2097 líneas), Scripts/scripts, img/logo.png (20925 bytes),
img/logo2.png (11929 bytes), documentación y una utilidad geográfica; esta última no pertenece a la interfaz.
No hay código de Firma Digital en estos checkouts; la propuesta de migración será representativa, sin modificar esa app.

| Existente | Decisión / mejora | Compatibilidad | Archivos |
|---|---|---|---|
| Compartido: variables, tipografías, modales de siete tamaños | Reutilizar; style.css importa un módulo rf- | Conservar reglas actuales | Styles/style.css, Styles/racafe-forms.css |
| Compartido: tooltips estáticos Scripts/scripts | Delegación para contenido dinámico y protección contra duplicados | Mantener ruta y Bootstrap 3/4 | Scripts/scripts |
| Compartido: logo.png y logo2.png | Publicar solo img, Styles y Scripts; no la raíz | No copiar imágenes al paquete | R/00_shared_assets.R |
| racafeFormsDependencies | Conservar dependencia base y sus rutas | Funciona sin Compartido para componentes anteriores | R/01_layout.R |
| Sin cargador local de Compartido | racafeSharedAssets con versión, CSS/JS opcionales y rutas acotadas | Adopción explícita | R/00_shared_assets.R |
| racafeForm / Row / Footer | Contenedor sin borde, ID opcional y acciones agrupadas | Argumentos opcionales al final | R/01_layout.R, R/08_actions.R |
| Sin panel genérico | racafePanel sin altura ni icono obligatorio | Nueva API independiente | R/05_panels_cards.R |
| racafeShiny: CajaIco, CajaValor | No depender de racafeShiny: tienen iconos y alturas obligatorios | Añadir composición genérica y clic optativo | R/05_panels_cards.R |
| Sin cuadrículas informativas genéricas | racafeCard, Metric, MetricGrid, InfoGrid, InfoItem | 1 a 4 columnas, móvil | R/05_panels_cards.R |
| racafeShiny: mostrarModal y clases en Compartido | Constructor racafeModal que usa esas mismas clases, sin copiar anchos | showModal/removeModal nativos | R/06_modals.R |
| racafeButton / ButtonDownload | Reutilizar botones y downloadHandler nativos, sin clics simulados | Mantener firmas | R/03_buttons_alerts.R, R/08_actions.R |
| Sin portapapeles central | UI de copia directa y mensajes R para texto/PNG, resultado real a Shiny | IDs locales en servidor; permisos HTTPS | R/07_clipboard.R, Compartido/Scripts/clipboard.js |
| Sin campo de referencia | racafeInputReadOnly con copia opcional | No publicar valores adicionales en logs | R/02_inputs.R |
| FormMessage / Alert | Extender estados y detalles técnicos explícitos, accesibilidad | Conservar tipos y argumentos anteriores | R/01_layout.R, R/03_buttons_alerts.R |
| Text / Select / Numeric / Date / DateRange | Extender estado requerido, error, ayuda y deshabilitado; texto con catálogo | Añadir opciones al final; conservar bindings y actualizadores | R/00_utils.R, R/02_inputs.R |
| Sin reset para todos los widgets | Reset por binding nativo con instantánea local del formulario | Acotado a formulario con ID; no reglas de negocio | R/04_updates.R, Compartido/Scripts/forms.js |
| CSS/JS base rf- en racafeForms | No reorganizar ni duplicar recursos anteriores | Estilo base anterior conservado | inst/www sin copia de nuevos estilos/scripts |

Los componentes nuevos requieren racafeSharedAssets() en la UI. La dependencia base sigue siendo
racafeFormsDependencies(); no se añaden dependencias R. Compartido es la única fuente de los nuevos estilos e interacciones.
Los colores nuevos reutilizan variables corporativas existentes; el rojo base anterior del paquete no cambia silenciosamente.
Los siete anchos de modal se toman de las reglas existentes, sin una segunda tabla CSS.
