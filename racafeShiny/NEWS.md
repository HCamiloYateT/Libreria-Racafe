# 1.4.1 — Encabezado fino

- Ajusta a 1px el grosor predeterminado del encabezado Vancouver; continúa siendo parametrizable.

# 1.4.0 — Tablas Vancouver

- Añade racafeTableVancouver sin cambiar las tablas anteriores: línea de encabezado, alineación y totales configurables.
- Reutiliza racafeCompactTable y gt_minimal_style; los recursos y el ancho mínimo se acotan a cada tabla gt.
- Incluye formato numérico con punto decimal/coma de miles, ayuda RStudio, ejemplos y pruebas.

# 1.3.1 — Módulo CajaValor

- Traslada la implementación UI/server a racafeModulos 0.3.0.
- Conserva nombres y argumentos anteriores como alias; racafeModulos es una dependencia opcional para esos alias.
- Mantiene el formato numérico en racafeShiny y añade pruebas de compatibilidad/reactividad.

# 1.3.0 — Navbar y detalle corporativo

- Listas seleccionables, grupos, tablas compactas gt/HTML y overlay por contenedor; reutiliza estilos y loaders existentes.

# Desarrollo (sin publicar)

- Añade racafeCollapsible con details/summary, triángulo rojo, teclado y estado Shiny.
- Añade racafeUpdateCollapsible para abrir/cerrar sin recrear contenido ni duplicar namespaces.
- Añade racafeModalFooter, adaptador del footer de racafeForms con estilos acotados.
- Conserva APIs y estilos previos; no se cambia el footer predeterminado de modales existentes.

# racafeShiny 1.2.0

- Amplía los loaders a doce operaciones sin cambiar sus métodos ni argumentos anteriores.
- Incluye logo2.png local por defecto; añade show_logo y respaldo para imágenes fallidas.
- Evita duplicar el automático con waiters existentes o el preloader nativo de bs4Dash.
- Limpia loaders corporativos al desconectar y conserva modales/notificaciones.
- Amplía demo, documentación y pruebas de variantes, recursos locales y operaciones reales de prueba.
- No simula progreso porcentual de tareas síncronas ni añade dependencias obligatorias.

# racafeShiny 1.1.0

- Añade racafePreloader con variantes inicio, actualizar y calculando basadas en PlantillaBs4Dash.
- Añade racafeLoaderUI para activación en el navegador antes de tareas bloqueantes y detección busy/idle.
- Añade objetos racafeLoader sobre waiter con show, update y hide, aislados por sesión y namespace.
- Incorpora recursos locales, movimiento reducido, cierre automático y demo modular Shiny/bs4Dash.

# Separadores numéricos

- Unifica los formatos predeterminados en punto decimal y coma de miles (1,234.56).
- Fija los separadores de controles y formatos para evitar diferencias regionales.

# Novedades

Este paquete sigue **Semantic Versioning (SemVer)**: `MAJOR.MINOR.PATCH`.

## Desarrollo

### Added
- `BotonDescarga()` acepta `label` para mostrar texto junto al icono del botón.
- `InputDinero()`, `InputPorcentaje()` e `InputNumero()` ofrecen controles
  `autoNumeric` compactos con etiqueta y formatos de moneda, porcentaje o
  numero simple.
- `ListaDesplegable()` acepta `size` para fijar el numero minimo de opciones
  visibles cuando el selector se repuebla dinamicamente.

## 1.0.0 - 2026-03-25

### Added
- Publicación inicial del paquete dentro del monorepo Racafe.
