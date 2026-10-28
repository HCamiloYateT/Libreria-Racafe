# Validación de racafeMail 0.1.0

Validación local realizada el 28 de octubre de 2026. Punto de partida: implementación del
ZIP `racafeMail_4_plantillas_20261028.zip` e instrucciones incluidas en
`email/INSTRUCCIONES_CODEX.md`. Se revisaron DESCRIPTION, NAMESPACE, ayuda, pruebas,
README y flujos CI de racafeForms/racafeShiny antes de crear el paquete independiente.

## Resultado

| Comprobación | Resultado |
|---|---|
| Pruebas testthat | 196 correctas; 0 fallos, advertencias u omisiones |
| Integración con los cuatro HTML reales de Compartido | 48 comprobaciones incluidas en las 196 |
| R CMD INSTALL | Correcto |
| R CMD build | Correcto |
| R CMD check --no-manual | Status: OK, sin omitir dependencias sugeridas |
| Ayuda del monorepo | Ocho paquetes; racafeMail cubre sus nueve funciones y el tema del paquete |
| Previews Chromium, 320 y 1440 px | 40 comprobaciones correctas; sin desborde horizontal |
| Sintaxis de workflows | YAML válido; Actions remoto aún no ejecutado |
| Envíos reales y credenciales reales | Ninguno utilizado |

Entorno: Linux, R 4.5.0, blastula 0.3.6, htmltools 0.5.8.1, testthat 3.2.3 y xml2 1.3.8.
La instalación de blastula se hizo desde el espejo CRAN en GitHub, commit
`d0a916ab62bb27881b78d40cbab68986fe68202c`, con sus dependencias disponibles en el entorno.
La prueba visual usa Chromium 154 y Playwright 1.62.1; intercepta las imágenes con el logo
local, sin consultar servidores externos ni acreditar disponibilidad de las URL públicas.

Compartido/email se valida con contrato 0.1.0 y commit
`d47401b5e1ae2292577dcda05def43ee051aa9f0`, fijado también en CI. No se modificaron los
componentes de racafeForms ni las aplicaciones/servicios de Firma Digital, FastAPI, MySQL u OEAPP.

## Cobertura funcional

- Configuración R única, precedencia de argumentos, variable de ruta, versión exacta y contrato compatible.
- Archivos ausentes, nombres no permitidos, traversal y symlinks fuera de la raíz.
- Validación de colores hexadecimales, HTTPS sin credenciales y direcciones sin inyección de cabeceras.
- Cuatro plantillas con firma y marca comunes; personalización de logo, firma, colores y pie.
- Escape de etiquetas y atributos, comillas, Unicode, ceros iniciales y datos con `{{...}}`/`[[...]]` literales.
- Sustitución de una pasada, sin ejecutar HTML/R de usuario; solo la base admite parciales internos.
- Variables faltantes/adicionales/duplicadas, NA, vectores, OTP inválido, vigencia fraccionaria y enlaces inseguros.
- Preview sin transporte, permisos 0600 Unix, protección de plantillas/credenciales y sobrescritura explícita.
- Compose real de blastula mediante su parámetro público `template`, sin anidar documentos HTML.
- SMTP y creds_file simulados; argumentos de envío, cuatro atajos, error genérico tipado y ausencia
  de payloads en stdout/mensajes/advertencias/condiciones. Importar el adaptador stdin no envía.
- HTML real parseado con xml2 y previews en dos anchos con imágenes locales de prueba.

La firma de dos columnas del ZIP desbordaba a 320 px. Se convirtió en una tabla de filas
apiladas y se limitó la tabla principal para mantener el contenido dentro del ancho disponible,
sin hojas de estilo externas ni media queries obligatorias.

## Reproducción

Desde la raíz del monorepo y con dependencias sugeridas instaladas:

```sh
export RACAFE_MAIL_TEST_HOME=/ruta/Compartido/email
Rscript -e 'testthat::test_local("racafeMail", stop_on_failure=TRUE)'
Rscript scripts/check_documentation.R
R CMD INSTALL racafeMail
R CMD build racafeMail
R CMD check --no-manual racafeMail_0.1.0.tar.gz
```

La integración externa solo se omite si no se define RACAFE_MAIL_TEST_HOME o falta xml2;
la ejecución registrada aquí no tuvo omisiones. Los fixtures unitarios son HTML sintético,
no plantillas corporativas duplicadas. Para reproducir la prueba visual opcional:

```sh
export RACAFE_MAIL_HOME=/ruta/Compartido/email
export RACAFE_MAIL_PREVIEW_DIR=/tmp/racafe-mail-previews
Rscript racafeMail/inst/examples/previews.R
npm install --prefix /tmp/racafe-mail-browser playwright@1.62.1
# Instale Chromium de Playwright o indique un ejecutable Chromium existente.
export NODE_PATH=/tmp/racafe-mail-browser/node_modules
export CHROMIUM_PATH=/usr/bin/chromium
export RACAFE_MAIL_TEST_LOGO=/ruta/Compartido/img/logo.png
node racafeMail/tests/browser/check-previews.cjs
```

La carpeta de salida debe ser nueva para no sobrescribir previews anteriores. No use datos
reales ni publique las vistas. La prueba visual complementa el parseo HTML; no reemplaza
pruebas de buzón o de clientes de correo.

## CID y entregabilidad

Se inspeccionó `blastula::compose_email()` 0.3.6 y su tratamiento de imágenes: conserva
las URL HTTP/HTTPS y puede convertir imágenes locales/data URI en partes CID. Esa conversión
no basta para afirmar portabilidad: sus identificadores y partes MIME pueden mostrarse como
adjuntos según el cliente. No se habilita CID ni data URI en la configuración pública de esta
versión, ni se reutilizan rutas del disco como URLs de email. El parámetro `template` permite
usar la estructura completa propia sin anidarla dentro de la plantilla predeterminada de blastula.

Antes de adoptar CID, probar con mensajes reales autorizados en Gmail web/móvil y Outlook
web/escritorio: logo y firma visibles, adjuntos inesperados, respuesta/reenvío, imágenes
bloqueadas, tamaño del mensaje, accesibilidad y charset UTF-8. Esas pruebas y la aceptación
SMTP real quedan pendientes; aquí no hay cuentas ni autorización para enviar correos reales.
Las URL HTTPS actuales también pueden bloquearse o fallar. La entrega depende de autenticación
SMTP, SPF/DKIM/DMARC, reputación y límites del proveedor. No se promete recepción ni apariencia
idéntica entre clientes. La expiración de OTP/tokens y los controles de negocio siguen en el backend.
