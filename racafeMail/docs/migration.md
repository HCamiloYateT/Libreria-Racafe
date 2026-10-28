# Migración propuesta y rollback

Este cambio no modifica Firma Digital, FastAPI, MySQL, Nginx, systemd, Shiny ni OEAPP.
La integración siguiente es una propuesta para otro PR, después de validar destinatarios
de prueba y clientes de correo.

## Preparación

1. Fijar el paquete y Compartido a commits aprobados, conservar los anteriores y registrar
   `packageVersion("racafeMail")`, `email/VERSION` y ambos SHAs.
2. Instalar el paquete y copiar `email/` a un directorio versionado. Revisar/configurar su
   lista R `racafe_mail`; protegerla como código de confianza, sin escritura del usuario web.
3. Configurar `RACAFE_MAIL_HOME`, `RACAFE_MAIL_FROM` y `RACAFE_MAIL_CRED` en el entorno
   apropiado sin registrar sus valores. El archivo de credenciales queda fuera del repositorio.
4. Generar las cuatro vistas previas con datos ficticios. Comparar firma, colores, texto,
   vigencias y enlaces. Después probar entrega con buzones autorizados de Gmail y Outlook,
   con imágenes bloqueadas/habilitadas, escritorio/móvil y lectores de pantalla.

## Firma Digital

En un PR futuro, sustituir el cuerpo de `shiny/misc/functions.R::firma_correo_html` por una
llamada a `racafeMail::racafeMailFirma(config)`, adaptando la firma real solo después de
inspeccionarla. Sustituir los bloques de correo por `racafeMailRender()` o el atajo de envío
correspondiente, preservando los eventos, permisos, validaciones y reglas del módulo.
No renderizar la base dentro de otra plantilla completa: la firma devuelve un fragmento;
el render devuelve un documento completo. No mantener dos motores en paralelo a largo plazo.

## FastAPI y OTP por entrada estándar

Conservar el contrato `(destino, código, nombre)`, el orden y los códigos de salida del script
existente `fastapi/scripts/enviar_otp.R`. Antes de cambiarlo, inspeccionar su formato real de
stdin: no se adjuntó ese archivo y no se presupone que sea idéntico al ejemplo.
`inst/examples/otp-stdin.R` propone tres líneas UTF-8 sin JSON ni argumentos CLI con secretos.
Solo se ejecuta al invocar el archivo directamente; hacer source() no envía.
Tras adaptar el lector al contrato existente, llamar a:

```r
racafeMail::racafeMailOTP(para = destino, codigo = codigo, nombre = nombre, config = config)
```

Mantener la generación, almacenamiento, verificación, vencimiento, rate limiting y token en
el backend. Este paquete solo valida formato y envía. Capturar `racafe_mail_smtp_error` y
devolver un mensaje/código genérico al navegador, nunca la condición del proveedor. No
reintentar indiscriminadamente ante fallos de red: la aceptación SMTP puede haber ocurrido.
La confirmación del correo no crea una firma, consentimiento ni escritura en base de datos.

## Activación y rollback

Activar primero para un grupo de prueba autorizado y observar únicamente contadores o estados
sin payloads. Conservar el adaptador anterior hasta validar el flujo completo. Si falla la
compatibilidad, detener el uso del adaptador nuevo, restaurar juntos el paquete, `email/` y
las referencias de configuración de la versión anterior y volver al flujo anterior.
No hay migración de base de datos ni cambio de servicios para revertir en este PR. Eliminar
previews con OTP/enlaces y no copiarlos a logs, artefactos públicos o reportes de incidentes.
