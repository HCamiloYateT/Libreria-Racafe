# Plantillas incluidas; no se leen archivos HTML externos por defecto ----
.rmail_templates <- list(
  base = paste(
    "<!doctype html>",
    "<html lang=\"es\">",
    "<head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>{{asunto}}</title></head>",
    "<body style=\"margin:0;background:{{color_fondo}};padding:0;\">",
    "<div style=\"display:none;max-height:0;overflow:hidden;opacity:0;\">{{preencabezado}}</div>",
    paste0(
      "<table role=\"presentation\" cellpadding=\"0\" cellspacing=\"0\" width",
      "=\"100%\" style=\"background:{{color_fondo}};border-collapse:collap",
      "se\">"
    ),
    "<tr><td align=\"center\" style=\"padding:24px 12px\">",
    "<table role=\"presentation\" cellpadding=\"0\" cellspacing=\"0\" width=\"600\"",
    paste0(
      "style=\"max-width:600px;width:100%;table-layout:fixed;background:",
      "#fff;border-collapse:collapse;border:1px solid #e8e8e8\">"
    ),
    "<tr><td style=\"padding:24px 28px 16px;border-top:5px solid {{color}};\">",
    paste0(
      "<img src=\"{{logo_url}}\" alt=\"{{empresa}}\" width=\"140\" style=\"dis",
      "play:block;max-width:100%;height:auto\">"
    ),
    "<p style=\"font-family:Arial,sans-serif;font-size:12px;color:#777;margin:12px 0 0\">{{categoria}}</p>",
    "</td></tr>",
    "<tr><td style=\"padding:8px 28px 12px\">[[CONTENIDO_INTERNO]]</td></tr>",
    "<tr><td style=\"padding:0 28px 18px\">[[FIRMA_INTERNA]]</td></tr>",
    "<tr><td style=\"border-top:1px solid #e8e8e8;padding:16px 28px 24px\">",
    "<p style=\"font-family:Arial,sans-serif;font-size:11px;line-height:1.5;color:#777;margin:0\">{{pie}}</p>",
    "[[AVISO_PRUEBAS]]",
    "</td></tr></table></td></tr></table></body></html>",
    sep = "\n"),
  invitacion = paste(
    paste0(
      "<div style=\"font-family:Arial,sans-serif;color:{{color_texto}};f",
      "ont-size:14px;line-height:1.6;overflow-wrap:anywhere\">"
    ),
    "<p>Cordial saludo, <strong>{{nombre}}</strong>:</p>",
    "<p>{{empresa}} le invita a revisar y diligenciar la autorizaci\u00f3n",
    "para el tratamiento de sus datos personales.</p>",
    "<p>Para continuar, acceda al siguiente enlace:</p>",
    "<table role=\"presentation\" cellpadding=\"0\" cellspacing=\"0\"><tr>",
    "<td style=\"background:{{color}};border-radius:5px;padding:12px 18px\">",
    "<a href=\"{{enlace}}\" style=\"color:#fff;text-decoration:none;font-weight:bold\">",
    "Abrir formulario de autorizaci\u00f3n</a></td></tr></table>",
    "<p><strong>Vigencia del enlace:</strong> {{vencimiento}}.</p>",
    "<p>El enlace es personal y no debe compartirse. Podr\u00e1 solicitarse un c\u00f3digo de verificaci\u00f3n",
    "enviado al correo registrado.</p><p>Cordialmente,</p></div>",
    sep = "\n"),
  otp = paste(
    paste0(
      "<div style=\"font-family:Arial,sans-serif;color:{{color_texto}};f",
      "ont-size:14px;line-height:1.6;overflow-wrap:anywhere\">"
    ),
    "<p>Cordial saludo, <strong>{{nombre}}</strong>:</p>",
    paste0(
      "<p>Hemos recibido una solicitud para verificar su correo electr\u00f3",
      "nico y continuar con la autorizaci\u00f3n"
    ),
    "para el tratamiento de sus datos personales.</p>",
    "<p>Ingrese el siguiente c\u00f3digo en el formulario:</p>",
    "<table role=\"presentation\" cellpadding=\"0\" cellspacing=\"0\"><tr>",
    "<td style=\"background:#f7f7f7;border:1px solid #e7e7e7;padding:16px 24px;text-align:center\">",
    "<span style=\"font-size:30px;font-weight:bold;letter-spacing:7px;color:{{color}}\">{{codigo}}</span>",
    "</td></tr></table>",
    "<p><strong>Vigencia del c\u00f3digo:</strong> {{vigencia_minutos}} minutos.</p>",
    paste0(
      "<p>Este c\u00f3digo es personal y no debe compartirse. Si no solicit\u00f3",
      " la verificaci\u00f3n, ignore el mensaje.</p>"
    ),
    "<p>Cordialmente,</p></div>",
    sep = "\n"),
  notificacion = paste(
    paste0(
      "<div style=\"font-family:Arial,sans-serif;color:{{color_texto}};f",
      "ont-size:14px;line-height:1.6;overflow-wrap:anywhere\">"
    ),
    "<p>Cordial saludo, <strong>{{nombre}}</strong>:</p>",
    "<p>{{mensaje}}</p>",
    "<p>Cordialmente,</p></div>",
    sep = "\n"),
  asignacion_crm = paste(
    '<p style="margin:0 0 6px;font-size:13px;color:{{color}};font-weight:bold">{{tipo}}</p>',
    '<h1 style="margin:0 0 18px;font-size:24px;line-height:1.2">{{titulo}}</h1>',
    '<table role="presentation" width="100%" cellpadding="12" cellspacing="0"',
    'style="background:#f6f7f9;border:1px solid #e3e6ea;table-layout:fixed">',
    '<tr><td width="50%"><small>CREADO POR</small><br><strong>{{creador}}</strong></td>',
    '<td><small>FECHA DE CREACIÓN</small><br><strong>{{fecha}}</strong></td></tr>',
    '<tr><td><small>ASIGNADO A</small><br><strong>{{responsable}}</strong></td>',
    '<td><small>PRIORIDAD</small><br><strong>{{prioridad}}</strong></td></tr>',
    '<tr><td colspan="2"><small>VENCIMIENTO</small><br><strong>{{vencimiento}}</strong></td></tr></table>',
    '<p style="margin:20px 0 6px;color:#505864;font-size:12px;font-weight:bold">DESCRIPCIÓN</p>',
    '<div style="white-space:pre-wrap;overflow-wrap:anywhere">{{descripcion}}</div>',
    '<p style="margin:20px 0 0;font-size:13px;color:#505864">Disponible en {{aplicacion}} · Notas y tareas.</p>',
    sep = "\n"),
  confirmacion = paste(
    paste0(
      "<div style=\"font-family:Arial,sans-serif;color:{{color_texto}};f",
      "ont-size:14px;line-height:1.6;overflow-wrap:anywhere\">"
    ),
    "<p>Cordial saludo, <strong>{{nombre}}</strong>:</p>",
    "<p>Confirmamos que se ha registrado la siguiente operaci\u00f3n:</p>",
    "<p style=\"background:#f7f7f7;border-left:4px solid {{color}};padding:12px 16px\">{{detalle}}</p>",
    "<p><strong>Fecha de confirmaci\u00f3n:</strong> {{fecha}}.</p>",
    "<p>Conserve este mensaje como referencia. Si no reconoce esta operaci\u00f3n, comun\u00edquese",
    "por los canales oficiales de Racaf\u00e9.</p>",
    "<p>Cordialmente,</p></div>",
    sep = "\n"),
  firma = paste(
    "<table role=\"presentation\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\"",
    "style=\"border-collapse:collapse;margin-top:12px\">",
    "<tr><td style=\"padding-bottom:10px\">",
    "<img src=\"{{firma_url}}\" width=\"150\" alt=\"Firma corporativa {{empresa}}\"",
    "style=\"display:block;max-width:100%;height:auto\"></td></tr>",
    paste0(
      "<tr><td style=\"font-family:Arial,sans-serif;font-size:13px;color",
      ":{{color_texto}};overflow-wrap:anywhere\">"
    ),
    "<strong>{{empresa}}</strong><br>",
    "<a href=\"{{web}}\" style=\"color:{{color}};overflow-wrap:anywhere\">{{web}}</a></td></tr></table>",
    sep = "\n")
)
