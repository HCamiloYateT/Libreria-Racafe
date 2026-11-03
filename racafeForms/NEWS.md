# Columnas nativas de formulario (sin publicar)

- Añade responsive = NULL/auto/stack/fixed a racafeFormRow, conservando el HTML y diseño heredados por defecto.
- Adapta las columnas al contenedor mediante CSS Grid y container queries, sin CSS de aplicación.
- Respeta columnas explícitas en el controlbar, con reducción de seguridad para controles muy estrechos.
- Amplía la galería, ayuda de RStudio y pruebas de columnas, densidades, namespaces y bindings.

# Correcciones de modal y controlbar (sin publicar)

- Precarga tamaños desde una fuente canónica de Compartido y evita apertura con CSS pendiente.
- Normaliza botones nativos de footers a rf-btn; cancelar secundario y confirmar principal rojo.
- Mantiene acciones horizontales a la derecha en móvil, conservando IDs y bindings.
- Evita falsos clics externos de pickers del controlbar, incluidos menús al body y renderUI.
- Conserva Bootstrap, selección múltiple, cierre normal y una sola instalación de listeners.
- Amplía galería, prueba fría del primer modal y regresiones de navegación/controles.

# Validación inline y selectores agregables (sin publicar)

- Añade asteriscos rojos en obligatorios y metadatos de obligatoriedad condicional.
- Unifica los modos de validación heredados en un único error por campo, sin resumen ni SweetAlert.
- Conserva una instancia de validación por formulario/sesión y permite liberarla con destroy.
- Añade uppercase a selectores sin modificar etiquetas originales ni valores; admite actualizaciones.
- Añade filas de campo/acción alineadas y excluye opciones de acción de Todos/conteos.
- Integra opcionalmente ListaDesplegableAgregable de racafeModulos en la galería, sin dependencia obligatoria circular.

# Galería y ampliación de componentes (sin publicar)

- Añade racafeFormsGallery con inventario completo, configuración por componente,
  código ejecutable/copiar, ayuda local y formulario comercial autocontenido.
- Añade variantes plain/soft/bordered sin alterar los defaults de bordered/unstyled.
- Amplía selectores con deduplicación opcional y conserva una sola opción vacía.
- Centraliza habilitación real/visual, también antes de renderUI, sin requerir shinyjs.
- Añade densidad/iconos a mensajes y resuelve Escape entre SweetAlert y Bootstrap.
- Incluye recursos de Compartido versionados sin CSS general, documentación y pruebas.
- No modifica aplicaciones productivas ni publica una nueva versión.

# Desarrollo (sin publicar)

- Unifica etiquetas, encabezados HTML compactos, separadores de grupos y footers.
- Añade validación de servidor reutilizable, reglas condicionales, resumen y foco,
  modos inline/alert/both y limpieza reactiva por formulario con namespaces.
- Añade racafeInputDateTime con POSIXct y zona horaria, estados de botones y
  descargas validadas que reutilizan el enlace nativo.
- Mantiene las firmas anteriores; amplía estados de switches, casillas y radios.
- Incluye una demo modular completa, pruebas y guía de migración.
- No incrementa la versión ni publica cambios.

# Separadores numéricos

- Unifica los formatos predeterminados en punto decimal y coma de miles (1,234.56).
- Fija los separadores de controles y formatos para evitar diferencias regionales.

# racafeForms 0.5.0

- Añade racafeInputFile con binding nativo, selección múltiple, estados y validación visual.
- Añade racafeValidateInputFile para validar metadatos, tamaño y lectura en el servidor.
- Añade racafeNotify y racafeRemoveNotify sobre notificaciones nativas, con namespaces y actualización por ID.
- Integra estilos y scripts acotados al paquete, sin nuevas dependencias ni cambios en Alert/Confirm.
- Incluye demo modular Shiny/bs4Dash, ayuda de RStudio y pruebas de cargas reales y notificaciones.

# racafeForms 0.4.2

- Elimina type antes de asignar email/tel y conserva los tipos HTML literales.
- Reutiliza el binding nativo con un adaptador limitado a campos propios no reconocidos por Shiny.
- Conserva la reactividad, namespaces y actualizaciones del servidor, sin afectar otros controles.

# racafeForms 0.4.1

- Corrige email/tel mediante tipo HTML text, inputmode y autocomplete.
- Añade inputmode y autocomplete adecuados sin sobrescribir autocomplete explícito.
- Verifica valores reactivos, namespaces, actualizaciones y estados de campo en Shiny y bs4Dash.

# racafeForms 0.4.0

- Añade racafeStatusBadge con variantes semánticas, texto escapado y tamaños xs/sm/md.
- Amplía tarjetas e indicadores con densidades y paneles/tarjetas con encabezado opcional.
- Usa Compartido 1.1.0 para altura por contenido, campos uniformes y estilos accesibles de badges.
- Documenta y verifica downloadHandler con icon = NULL o un único icono explícito.
- Amplía la demo con filtros, reactable y badges, conservando módulos, modales y portapapeles existentes.
- Conserva orden y nombres de los argumentos públicos de 0.3.0; solo añade opciones al final.

# racafeForms 0.3.0

- Los selectores múltiples usan checks rojos sobre filas neutras; la selección simple conserva su acento.
- Se documenta la política opcional de filtro vacío como «Todos (sin filtro)», sin modificar los valores Shiny.
- Añade cargador local versionado de Compartido, con publicación acotada a Styles, Scripts e img.
- Añade paneles, tarjetas, métricas, cuadrículas informativas y barras de acciones reutilizables.
- Añade modales que reutilizan los siete tamaños CSS existentes de Compartido.
- Añade copia de texto y PNG con resultado real del navegador, sin registrar contenido sensible.
- Añade campos readonly con copia optativa y reset de formularios por bindings nativos.
- Amplía argumentos opcionales de campos para ayuda, errores, requerido, deshabilitado y readonly cuando corresponde.
- Añade catálogo de sugerencias de texto, estados loading/empty y detalles técnicos explícitos y escapados.
- Conserva la API anterior y corrige el icono duplicado del botón de descarga, manteniendo downloadHandler nativo.
- Los estilos y scripts nuevos residen solo en Compartido; los recursos base anteriores se conservan.
- Incluye demo Shiny/bs4Dash, pruebas de navegador, matriz de compatibilidad y migración representativa.

# racafeForms 0.2.1

- Unifica la apariencia de los calendarios de fecha y rango: colores, fuente, bordes, sombra, dimensiones y navegación.
- Ambas vistas diarias comienzan en lunes y muestran seis semanas completas.
- Conserva Air Datepicker para fecha, Bootstrap Datepicker para rango y sus actualizadores nativos.
- Limita los estilos a instancias racafeForms y respeta las tres densidades del formulario.

# racafeForms 0.2.0

- Centra el contenido de los selectores y trunca etiquetas extensas sin cambiar la altura del control.
- Agrega `gender` y resúmenes de selección: Ninguno/Ninguna, etiqueta individual, conteo y Todos/Todas.
- Conserva selecciones válidas y vacías al actualizar opciones desde la sesión Shiny.
- Sincroniza el estado deshabilitado del selector y su botón al usar shinyjs.
- Cambia el separador del rango a `-` y distribuye las fechas con el mismo ancho y altura.
- Aplica el calendario corporativo solo a rangos racafeForms, también dentro de bs4DashControlbar.
- Adjunta CSS y JavaScript a los selectores y rangos sin requerir un contenedor de formulario.

# racafeForms 0.1.3

- Compacta el switch dentro de los formularios: pista de 28 px y botón de 16 px.

# racafeForms 0.1.2

- Reduce proporcionalmente los iconos de SweetAlert en alertas y confirmaciones.

# racafeForms 0.1.1

- Unifica altura visual de pickerInput y autonumericInput en 36 px.
- Verifica que la demostración de presupuesto usa `format = "cop"` (separadores colombianos).

# racafeForms 0.1.0

- Primera version: formularios, selects, numericos, fechas, rangos, switches,
  radio, checkbox, botones, descargas, SweetAlerts, actualizadores y tema CSS.
