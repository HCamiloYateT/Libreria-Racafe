# racafeMail 0.2.0

Correos corporativos **autocontenidos**: plantillas, configuración R, logotipo y firma
incluidos en el paquete. No requiere Compartido, `/home/compartido/email`, archivos HTML
externos ni configuración JSON. Renderizar y generar previews no hace SMTP ni descarga recursos.

## Instalación y uso

```r
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeMail",
                        ref = "codex/racafemail-self-contained", dependencies = NA, upgrade = "never")
# Desde un clon local: remotes::install_local("racafeMail", dependencies = NA, upgrade = "never")

config <- racafeMail::racafeMailConfig()
correo <- racafeMail::racafeMailRender("otp",
  list(nombre = "Ana", codigo = "012345", vigencia_minutos = 5L), config)
archivo <- tempfile(fileext = ".html")
racafeMail::racafeMailPreview("otp",
  list(nombre = "Ana", codigo = "012345", vigencia_minutos = 5L), archivo, config)
```

Para una instalación reproducible, sustituya la rama por el SHA completo aprobado del paquete.
Requiere R >= 4.1.0, blastula >= 0.3.6, htmltools y base64enc (ya es dependencia de blastula).
Los únicos recursos de ejecución propios son **dos archivos R y un PNG**. Todo viaja en la
instalación estándar de R; no copie carpetas adicionales después de instalar.

## Archivos y fuente única

El paquete pasa de **32 a 15 archivos fuente**, manteniendo las nueve funciones públicas,
las pruebas y los dos ejemplos. No se eliminó documentación de APIs: se agrupa en una ayuda
común, accesible por los nueve nombres y por `help(package = "racafeMail")`.

| Archivo/grupo | Responsabilidad |
|---|---|
| `R/racafeMail.R` | Configuración R, validación, render, preview y transporte; API documentada. |
| `R/resources.R` | Base HTML, firma y las cuatro plantillas como recursos R internos. |
| `inst/img/logo.png` | Única imagen incluida, reutilizada para logo y firma por defecto. |
| `inst/examples/` | Cuatro previews sin envío y adaptador stdin OTP propuesto. |
| `tests/testthat/test-racafeMail.R` | Pruebas consolidadas; no duplica plantillas corporativas. |
| `man/racafeMail.Rd` | Ayuda generada para las nueve funciones y el paquete. |

DESCRIPTION, NAMESPACE, LICENSE, NEWS, este README y los ejecutores de pruebas completan
la estructura estándar. La configuración inicial se define en R dentro del motor y se
personaliza por argumentos; no existe un config.R obligatorio ni un VERSION externo.
La versión de recursos incluidos es la versión del paquete.

## Personalización de identidad

```r
config <- racafeMail::racafeMailConfig(
  empresa = "Racafé & Cía. S.C.A.", web = "https://www.racafe.com",
  color = "#DA291C", color_fondo = "#f4f4f4", color_texto = "#333333",
  pie = "Mensaje automático. Por favor, no responda."
)
# Opcional: un PNG propio se incorpora al HTML; no queda una referencia al disco.
# config <- racafeMail::racafeMailConfig(logo_url = "/ruta/mi-logo.png")
```

`logo_url` y `firma_url` aceptan PNG local, PNG embebido o URL HTTPS. Por defecto usan el
PNG incluido, sin ninguna URL de GitHub. Si pasa solo logo_url, la firma sigue ese logo;
si especifica firma_url, puede usar otra imagen. Una URL HTTPS es una elección opcional:
el cliente de correo deberá cargarla. Para conservar la independencia de red, use PNG local
o el incluido. Se excluye SVG/HTML arbitrario y se validan URL, colores y valores.

En previews, las imágenes van embebidas como data URI. Al componer un correo SMTP,
blastula las convierte en partes **CID**; logo y firma iguales producen una sola imagen MIME.
Se verificó la composición y la visualización en Chromium. La presentación real en Gmail,
Outlook, reenvíos y adjuntos depende del cliente y aún requiere pruebas con buzones autorizados.
No se garantiza apariencia idéntica o entrega por haber generado el HTML correctamente.

## API y datos de las plantillas

| Función | Resultado |
|---|---|
| `racafeMailConfig()` | Lista de configuración validada. Funciona sin argumentos ni rutas. |
| `racafeMailFirma(config)` | Fragmento de firma HTML, sin SMTP. |
| `racafeMailRender(plantilla, datos, config)` | `list(asunto, html)`, sin SMTP. |
| `racafeMailPreview(plantilla, datos, archivo, config)` | Archivo HTML UTF-8 privado, sin SMTP. |
| `racafeMailEnviar(para, plantilla, datos, config)` | Envío explícito mediante blastula. |
| `racafeMailOTP(para, codigo, nombre, minutos, config)` | Atajo para OTP de seis dígitos. |
| `racafeMailInvitacion(para, nombre, enlace, vencimiento, config)` | Atajo para invitación HTTPS. |
| `racafeMailNotificacion(para, nombre, mensaje, config)` | Atajo para notificación de texto. |
| `racafeMailConfirmacion(para, nombre, detalle, fecha, config)` | Atajo para confirmación genérica. |

| Plantilla interna | Datos requeridos |
|---|---|
| `invitacion` | nombre, enlace HTTPS, vencimiento |
| `otp` | nombre, codigo de texto de seis dígitos, vigencia_minutos entero positivo |
| `notificacion` | nombre, mensaje |
| `confirmacion` | nombre, detalle, fecha legible |

Los previews pueden guardarse como invitacion.html, otp.html, notificacion.html y confirmacion.html;
no se necesitan esos archivos en el disco para renderizar. Todos los valores son escalares no NA;
solo nombre puede estar vacío. No se acepta HTML de usuario. El escape de una pasada evita
reinterpretar marcadores insertados desde los datos. Se rechazan variables faltantes, extra,
duplicadas y marcadores sin resolver en las plantillas. Los enlaces no consumen/verifican tokens.
La generación, expiración y rate limiting de OTP siguen siendo responsabilidad del consumidor.

## SMTP y credenciales

Las credenciales no se incluyen en el paquete ni en Git. Configure `RACAFE_MAIL_FROM` y
`RACAFE_MAIL_CRED` (ruta a credenciales creadas con `blastula::create_smtp_creds_file()`),
o páselas explícitamente. Se mantienen los fallback FIRMA_BLASTULA_FROM y FIRMA_BLASTULA_CRED.
RACAFE_MAIL_TEST_MODE/FIRMA_TEST_MODE agregan el aviso de pruebas, pero **no bloquean un envío
explícito**. Ningún render, preview ni instalación envía correos automáticamente.

```r
# ENVÍA correo: usar solo con un destinatario que deba recibirlo.
# racafeMail::racafeMailOTP("destinatario@example.org", "012345", nombre = "Persona")
```

El transporte usa smtp_send y creds_file, sin reintentos automáticos. TRUE invisible significa
transporte completado sin error, no recepción en el buzón. Un fallo produce
`racafe_mail_smtp_error` sin adjuntar el error del proveedor ni datos sensibles. Evite reintentar
sin control: una desconexión puede ocurrir después de la aceptación SMTP. SPF/DKIM/DMARC,
reputación y límites del proveedor siguen siendo necesarios para la entrega.

Los previews pueden contener OTP/enlaces: no publique sus archivos ni los imprima en logs.
Se crean con permisos 0600 en Unix y no se sobrescriben por defecto. overwrite=TRUE no permite
reemplazar credenciales o recursos instalados. Los tests solo usan datos ficticios.

## Migración y rollback

Para adoptar 0.2.0, instale el paquete y use `racafeMailConfig()` sin ruta. Deje de hacer source()
del motor/configuración externos y pase cualquier personalización anterior como argumentos R.
RACAFE_MAIL_HOME ya no cambia automáticamente la ruta: esto impide que una variable heredada
reactive accidentalmente una dependencia externa. `version="0.2.0"` permite exigir esta versión.

La API conserva nombres y orden de argumentos. Para compatibilidad optativa, una ruta explícita
`racafeMailConfig(ruta = "/ruta/email", version = "0.1.0")` sigue admitiendo el contrato externo
anterior, con protección contra traversal/symlinks. Esa ruta es R/HTML de confianza y su uso
renuncia a la independencia de esa carpeta. No es necesaria para ninguna función del paquete.

No se han eliminado carpetas de Compartido ni modificado Firma Digital, FastAPI, MySQL u OEAPP.
Los consumidores 0.1.x pueden continuar usando sus recursos anteriores. Para rollback reinstale
el commit previo 0.1.x y restaure sus referencias de ruta/configuración; no hay migración de datos.

El ejemplo otp-stdin.R es una propuesta, no sustituye scripts de FastAPI: conserva destino,
código y nombre, una línea UTF-8 por campo. Hacer source() no envía; ejecutarlo directamente sí.
Antes de integrarlo, adapte el lector al contrato real del servicio en otro PR.

## Validación y reproducción

```sh
Rscript -e 'testthat::test_local("racafeMail", stop_on_failure=TRUE)'
Rscript scripts/check_documentation.R
R CMD INSTALL racafeMail
R CMD build racafeMail
R CMD check --no-manual racafeMail_0.2.0.tar.gz
RACAFE_MAIL_PREVIEW_DIR=/tmp/racafe-mail-previews Rscript racafeMail/inst/examples/previews.R
```

La carpeta de previews debe ser nueva. El ejemplo funciona sin RACAFE_MAIL_HOME ni Compartido.
Para revisión visual opcional, instale Playwright 1.62.1 en un entorno de desarrollo, configure
CHROMIUM_PATH si usa un Chromium existente y ejecute
`RACAFE_MAIL_PREVIEW_DIR=/tmp/racafe-mail-previews node racafeMail/tests/browser/check-previews.cjs`.
La prueba bloquea toda red externa y valida las imágenes embebidas reales a 320/1440 px.

Se instaló el tarball en otra biblioteca R y se renderizaron las cuatro plantillas desde
un directorio vacío, con RACAFE_MAIL_HOME apuntando a una ruta inexistente.
Las pruebas de recursos internos se ejecutan siempre, sin variables de rutas ni omisiones
por ausencia de Compartido. CI ya no clona ese repositorio ni necesita COMPARTIDO_READ_TOKEN.
Se conservan pruebas de seguridad, transporte simulado y compatibilidad explícita con rutas.
No se enviaron correos reales ni se usaron credenciales reales durante la validación.

Validación local de 0.2.0 (28 de octubre de 2026): **247 comprobaciones testthat**,
0 fallos/advertencias/omisiones; **42 comprobaciones de Chromium** con imágenes reales
embebidas y red bloqueada; R CMD INSTALL/build/check --no-manual: **OK**. Ayuda de las
nueve funciones y sintaxis YAML correctas. Se verificó que el HTML de los seis recursos
internos es idéntico al HTML previamente validado y que logo/firma se deduplican en CID.
Entorno: R 4.5.0, blastula 0.3.6, htmltools 0.5.8.1, testthat 3.2.3,
Playwright 1.62.1 y Chromium 154. Gmail/Outlook y entrega SMTP real siguen pendientes.
