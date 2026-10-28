# racafeMail

Paquete independiente para correos corporativos con **configuración R**, plantillas locales
de `Compartido/email` y transporte explícito mediante `blastula`. Requiere R >= 4.1.0.
El renderizado, la firma y las vistas previas no envían correo. No depende de `racafeForms`
ni modifica Firma Digital, FastAPI, MySQL o los servicios de OEAPP.

## Instalación

```r
install.packages("remotes") # Solo si no está instalado.
remotes::install_github("HCamiloYateT/Libreria-Racafe", subdir = "racafeMail",
                        ref = "codex/racafe-mail", dependencies = NA, upgrade = "never")
```

Para una instalación reproducible, sustituya `ref` por el **SHA completo aprobado** del
paquete y fije también el commit de Compartido. Desde un clon local:

```r
remotes::install_local("racafeMail", dependencies = NA, upgrade = "never")
```

Las dependencias de ejecución son `htmltools` y `blastula >= 0.3.6` (con sus dependencias).
No se distribuyen copias de las plantillas de producción en el paquete. Prepare una copia
versionada de Compartido y apunte a su carpeta `email`:

```sh
git clone --branch codex/racafe-mail-templates https://github.com/HCamiloYateT/Compartido.git vendor/Compartido
git -C vendor/Compartido checkout --detach d47401b5e1ae2292577dcda05def43ee051aa9f0
# Compruebe git rev-parse HEAD y registre ambos commits antes de liberar.
export RACAFE_MAIL_HOME="$PWD/vendor/Compartido/email"
```

`email/VERSION` debe ser 0.1.x; `version = "0.1.0"` exige coincidencia exacta del contrato.
La versión no reemplaza el pin del commit: un despliegue reproducible debe registrar ambos
SHAs. No se hace fetch HTTP durante el renderizado ni durante el envío. Las imágenes
HTTPS quedan referenciadas para que las cargue el cliente de correo.

## Configuración R única

`racafeMailConfig()` carga la lista `racafe_mail` de `email/config.R`; no hay configuración
JSON. El archivo es **R de confianza**, administrado por el operador y no editable por usuarios
finales. Los argumentos explícitos prevalecen sobre el archivo:

```r
config <- racafeMail::racafeMailConfig(
  version = "0.1.0", empresa = "Racafé & Cía. S.C.A.", color = "#DA291C",
  color_fondo = "#f4f4f4", color_texto = "#333333",
  logo_url = "https://example.org/marca/logo.png",
  firma_url = "https://example.org/marca/firma.png",
  pie = "Mensaje automático. Por favor, no responda."
)
```

Use URL HTTPS estables/públicas para `logo_url`, `firma_url` y `web`; nunca rutas relativas
o del disco. No se comprueba disponibilidad remota para no generar peticiones por correo.
Si se cambia solo `logo_url` por argumento, la firma sigue ese logo salvo `firma_url` explícito.
Los colores admiten `#RRGGBB` y se aplican a todas las plantillas. La firma es un parcial
compartido; puede cambiar su HTML local de confianza conservando sus marcadores.

Las referencias SMTP vienen de `RACAFE_MAIL_FROM` y `RACAFE_MAIL_CRED`, esta última una ruta
a un archivo externo creado con `blastula::create_smtp_creds_file()`. El paquete no crea,
imprime ni guarda contraseñas. `RACAFE_MAIL_TEST_MODE=true` agrega un aviso de pruebas.
Por compatibilidad, `config.R` usa como fallback `FIRMA_BLASTULA_FROM`, `FIRMA_BLASTULA_CRED`
y `FIRMA_TEST_MODE`; los nombres nuevos tienen precedencia. El aviso de pruebas **no evita
el envío** si se llama expresamente a una función de envío. Mantenga las credenciales fuera
de Git y de la carpeta de recursos; el proceso solo necesita permiso de lectura.

## Cuatro plantillas y API

| Función | Uso |
|---|---|
| `racafeMailConfig()` | Cargar y validar configuración y archivos locales. |
| `racafeMailFirma()` | Renderizar la firma común, sin SMTP. |
| `racafeMailRender()` | Devolver `list(asunto, html)` de una plantilla, sin SMTP. |
| `racafeMailPreview()` | Guardar HTML UTF-8 local, sin SMTP ni apertura de navegador. |
| `racafeMailEnviar()` | Enviar explícitamente con `smtp_send()` y `creds_file()`. |
| `racafeMailOTP()` | Atajo de envío: destino, código, nombre y minutos. |
| `racafeMailInvitacion()` | Atajo de envío: nombre, enlace y vencimiento. |
| `racafeMailNotificacion()` | Atajo de envío: nombre y mensaje. |
| `racafeMailConfirmacion()` | Atajo de envío: nombre, detalle y fecha. |

| Plantilla | Datos requeridos |
|---|---|
| `invitacion.html` | `nombre`, `enlace` HTTPS, `vencimiento` |
| `otp.html` | `nombre`, `codigo` de texto con seis dígitos, `vigencia_minutos` entero positivo |
| `notificacion.html` | `nombre`, `mensaje` de texto plano |
| `confirmacion.html` | `nombre`, `detalle` de texto plano, `fecha` legible |

Todos los valores son escalares no NA. Solo `nombre` puede estar vacío. Se rechazan variables
faltantes, adicionales, duplicadas, códigos inválidos, URLs no HTTPS y marcadores no resueltos
en las plantillas. Los tokens remotos no se verifican ni consumen: el backend conserva su
responsabilidad por autorización, expiración y rate limiting. Las fechas son textos legibles;
no se infiere zona horaria ni formato. No se admite HTML de usuario, ni siquiera con `HTML()`.

## Renderizado y preview sin envío

```r
config <- racafeMail::racafeMailConfig(version = "0.1.0")
datos <- list(nombre = "Ángela", codigo = "012345", vigencia_minutos = 5L)
correo <- racafeMail::racafeMailRender("otp", datos, config)
archivo <- tempfile(fileext = ".html")
racafeMail::racafeMailPreview("otp", datos, archivo, config)
# Revise localmente archivo y elimínelo después; contiene el código de prueba.
```

`inst/examples/previews.R` genera las cuatro vistas con datos ficticios en una carpeta
nueva del directorio de trabajo; `RACAFE_MAIL_PREVIEW_DIR` permite elegir otra ruta. Los previews se
crean con permisos 0600 en sistemas Unix. No se sobrescriben archivos por defecto:
`overwrite = TRUE` debe ser explícito y nunca permite reemplazar recursos ni credenciales.
El HTML y el resultado del render contienen los datos: no los imprima en logs ni los exponga
en rutas web públicas. Los errores de validación no incluyen los valores recibidos.

## Envío explícito

Configure las referencias SMTP fuera del código y ejecute únicamente con un destinatario
que deba recibir la comunicación:

```r
# Ejemplo intencional de envío: NO se ejecuta en tests ni en previews.
config <- racafeMail::racafeMailConfig()
racafeMail::racafeMailOTP("destinatario@example.org", "012345", nombre = "Persona", config = config)
```

Los atajos y `racafeMailEnviar()` devuelven TRUE invisible si el transporte termina sin error,
no una confirmación de recepción. Se acepta una dirección simple por llamada; no se admiten
cabeceras, nombres de presentación ni listas separadas por comas. El transporte no reintenta
automáticamente. Si falla, capture `racafe_mail_smtp_error`; su mensaje es genérico y no
adjunta el error SMTP original. No registre OTP, enlaces personales, credenciales ni datos del
correo. Una desconexión puede ocurrir después de aceptar el mensaje: reintentar sin control
de negocio puede duplicar envíos.

## Ayuda, pruebas y migración

```r
help(package = "racafeMail")
?racafeMail::racafeMailRender
```

Desde la raíz del monorepo, con dependencias de pruebas instaladas:

```sh
RACAFE_MAIL_TEST_HOME=/ruta/Compartido/email Rscript -e 'testthat::test_local("racafeMail", stop_on_failure=TRUE)'
Rscript scripts/check_documentation.R
R CMD build racafeMail
RACAFE_MAIL_TEST_HOME=/ruta/Compartido/email R CMD check --no-manual racafeMail_0.1.0.tar.gz
```

Sin `RACAFE_MAIL_TEST_HOME`, solo la integración con las plantillas externas se omite; las
pruebas unitarias usan HTML sintético, no duplican las plantillas corporativas. No hay envíos
reales en las pruebas. CI fija el commit `d47401b5e1ae2292577dcda05def43ee051aa9f0` de Compartido y ejecuta también
esa integración. Si Compartido es privado, configure `COMPARTIDO_READ_TOKEN` en los secrets
de Actions con acceso de lectura a ese repositorio; de lo contrario se usa `github.token`.

Vea [migración y rollback](docs/migration.md) y [validación y compatibilidad de correo](docs/validation.md).
CID se ha investigado, pero no se habilita hasta probarlo en Gmail/Outlook. Las imágenes remotas
pueden bloquearse; permanecen el texto y los atributos alt. Tablas y estilos inline mejoran
compatibilidad, sin garantizar apariencia idéntica entre clientes. La entrega requiere SMTP
válido, SPF/DKIM/DMARC y reputación del remitente; no se ha probado en buzones reales.
