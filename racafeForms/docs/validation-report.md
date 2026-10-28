# Validación y compatibilidad de componentes corporativos

## Alcance implementado

- racafeForms 0.3.0: 15 funciones nuevas (45 públicas en total), extensiones opcionales de campos,
  mensajes y footers; ayuda generada, ejemplos, pruebas y migración representativa.
- Compartido 1.0.0: manifiesto ASSETS_VERSION, módulo CSS rf-, scripts reutilizables para copia y reset,
  tooltips dinámicos e instrucciones de carga local. Las reglas previas de style.css permanecen idénticas;
  solo se añadió el import del módulo. No se modificaron las imágenes ni utilidades geográficas.
- Independencia respecto a racafeShiny conservada. No hay nuevos Imports ni Suggests de R.
- Firma Digital y CRM Nacional no se modificaron. No se añadieron clientes HTTP de negocio, SQL, SMTP,
  credenciales, tokens, hashes de solicitudes, generación de evidencias ni rutas particulares de OEAPP.

## Resultados ejecutados

| Comprobación | Resultado |
|---|---|
| testthat de racafeForms | 237 comprobaciones pasan; 0 fallos, 0 advertencias, 0 omisiones. |
| R CMD check --no-manual | OK. Se usó _R_CHECK_FORCE_SUGGESTS_=false; devtools no está instalado. |
| R CMD INSTALL | Instalación y carga correctas. |
| Ayuda y documentación | 45 funciones públicas de racafeForms documentadas; verificador de los siete paquetes OK. |
| Chromium con Shiny / Bootstrap 3 | 54 comprobaciones pasan. |
| Chromium con bs4Dash / Bootstrap 4 | 54 comprobaciones pasan. |
| Demo anterior sin Compartido | Carga, bindings y confirmación SweetAlert nativa correctos. |
| Copia real | Texto y PNG presentes en el portapapeles real de Chromium. |
| Copia fallida / pendiente | Permiso denegado informa fallo; promesa pendiente no informa éxito anticipado. |
| Recursos sin red externa | Sin peticiones externas en ambas demos. |
| Carga repetida / dinámica | Dependencias HTML deduplicadas; script de copia no duplica los manejadores. |
| Modales | Siete tamaños exactos en escritorio; pie/encabezado opcionales; cierre controlado; móvil sin desbordamiento. |
| Formularios | Dos módulos simultáneos, reset/limpieza por binding, readonly conservado y descarga nativa verificados. |
| Código | Sintaxis JS, documentación, git diff --check y límite de 120 caracteres en R/ y nuevos ejemplos correctos. |

Entorno probado: Debian 13, R 4.5.0, Shiny 1.10.0, shinyWidgets 0.9.1, htmltools 0.5.8.1,
bs4Dash 2.3.5, Playwright 1.62.1 y Chromium 154.0.8037.57. No se afirma validación en otros navegadores.
El portapapeles se probó en localhost con permisos concedidos y con una denegación simulada explícita.
Los modos HTTPS remotos y otros navegadores siguen sujetos a sus permisos y compatibilidad nativos.

La demo bs4Dash suprime sus dependencias CDN opcionales ionicons/googlefonts; los componentes usan
iconos locales y alternativas de fuentes. Los scripts de Compartido no realizan descargas externas
salvo que la aplicación solicite explícitamente copiar una imagen de una URL externa.

## Matriz de versiones y adopción

| Consumidor | Compartido | Compatibilidad |
|---|---|---|
| racafeForms <= 0.2.1 | No requerido para su API base | Sin cambios en esos paquetes publicados. |
| racafeForms 0.3.0, API previa | No cargado | Argumentos anteriores conservados en el mismo orden; prueba fija de 30 firmas. |
| racafeForms 0.3.0, componentes nuevos | 1.0.0 | Requiere racafeSharedAssets en la UI, una configuración por página. |
| racafeForms 0.3.0, UI con recursos compartidos | 1.0.0 | Adopción explícita de paleta y fuentes corporativas de Compartido. |
| Revisión vieja sin ASSETS_VERSION | Cargador nuevo | Error explícito; actualizar la copia local, no se descarga automáticamente. |

El cargador compara la versión declarada; no reemplaza la fijación del commit durante el despliegue.
Se recomienda revisar e integrar primero el PR de Compartido y después el PR de Libreria-Racafe.
No se ha integrado ninguna de las ramas en main ni publicado una versión de producción.

## Pruebas reproducibles

1. Instalar racafeForms desde el checkout y disponer de Compartido 1.0.0 en una ruta local.
2. Definir RACAFE_SHARED_PATH o la opción racafeForms.shared_path.
3. Ejecutar testthat::test_local("racafeForms", stop_on_failure = TRUE) desde la raíz del monorepo.
4. Iniciar inst/examples/corporate-components.R con shiny::runApp().
5. En tests/browser, npm ci; npx playwright install chromium; npm test.
6. RACAFE_TEST_URL permite apuntar al puerto elegido y CHROMIUM_PATH a Chromium ya instalado.
7. Repetir la demo con RACAFE_DEMO_BS4DASH=1 para el modo bs4Dash.

La aplicación demostrativa es autocontenida y usa datos ficticios. Las pruebas no requieren
Firma Digital, bases de datos, correo o servicios de firma. Las métricas de reducción de código
son representativas: 39 a 13 líneas, sin blancos/comentarios (66,7 % menos); no se midieron los módulos reales.

## Revisión de los cambios

La matriz de componentes y decisiones está en component-matrix.md. El ejemplo completo antes/después
se encuentra en migration/. Las ramas de ambos repositorios se preparan para revisión; la integración
con main y el despliegue controlado requieren revisión del propietario del proyecto.
