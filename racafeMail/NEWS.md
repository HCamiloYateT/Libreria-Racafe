# racafeMail 0.1.0

- Añade nueve funciones públicas para configuración R, firma, render, preview y envío de cuatro plantillas.
- Centraliza el motor en el paquete y deja HTML/configuración en Compartido/email 0.1.0.
- Escapa variables en una pasada y restringe parciales, rutas, URLs y colores.
- Separa SMTP de previews y sanea errores del proveedor sin registrar contenido sensible.
- Añade ayuda RStudio, ejemplos, pruebas unitarias/integración y propuesta de migración y rollback.

## Diferencias frente al ZIP inicial

- config.R define una lista racafe_mail; no define ni duplica racafeMailConfig().
- Se conservan nombres y orden de los argumentos iniciales; los valores visuales se cargan del archivo R.
- Se añaden overrides de marca, colores y versión; defaults NULL significan «usar config.R».
- OTP exige código de texto para preservar ceros iniciales; vigencias fraccionarias no se truncan.
- Se rechazan variables adicionales y nombres duplicados para detectar errores de integración.
- Preview no sobrescribe por defecto; overwrite = TRUE es explícito y protege recursos/credenciales.
- SMTP usa direcciones simples y devuelve errores saneados de clase racafe_mail_smtp_error.
