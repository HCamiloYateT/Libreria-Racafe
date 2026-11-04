"""Preparar copias y parches UI de los seis adjuntos sin ejecutar código del CRM."""
from pathlib import Path
import argparse
import difflib
import re


def closing(text, start, opening='(', closing=')'):
    depth, quote, escaped, comment = 0, None, False, False
    for index in range(start, len(text)):
        char = text[index]
        if comment:
            if char == '\n': comment = False
            continue
        if quote:
            if escaped: escaped = False
            elif char == '\\': escaped = True
            elif char == quote: quote = None
            continue
        if char == '#': comment = True
        elif char in ('"', "'", '`'): quote = char
        elif char == opening: depth += 1
        elif char == closing:
            depth -= 1
            if depth == 0: return index + 1
    raise ValueError('Bloque sin cerrar')


def replace_function(text, name, replacement):
    match = re.search(re.escape(name) + r'\s*<-\s*function\(', text)
    if not match: raise ValueError(name)
    args_end = closing(text, text.index('(', match.start()))
    body_start = text.index('{', args_end)
    end = closing(text, body_start, '{', '}')
    return text[:match.start()] + replacement.rstrip() + text[end:]


def remove_call(text, token):
    start = text.index(token)
    end = closing(text, text.index('(', start))
    if text[end:end+1] == ',': end += 1
    return text[:start] + text[end:]


def migrate(name, text):
    if name == '00_header.R':
        text = replace_function(text, '.header_badge', '''.header_badge <- function(nombre, valor) {
  racafeForms::racafeDataBadge(sub("[[:space:]:]+$", "", nombre), valor, variant = "neutral", size = "xs")
}''')
        text = replace_function(text, '.header_tabla', '''.header_tabla <- function(datos) {
  racafeShiny::racafeCompactTable(datos, backend = "html", max_height = "190px")
}''')
        text = replace_function(text, '.header_lista_item', '''.header_lista_item <- function(titulo, detalle, badges = NULL, id = NULL, value = NULL) {
  racafeShiny::racafeListItem(titulo, subtitle = detalle, badges = badges, id = id, value = value)
}''')
        text = re.sub(r'''onclick = sprintf\(\s*"event.stopPropagation\(\);Shiny.setInputValue\('%s',%s,\{priority:'event'\}\);",\s*input_id, jsonlite::toJSON\(list\(nit = nit, linneg = as.character\(fila\$LinNegCod\[\[1\]\]\)\), auto_unbox = TRUE\)\s*\)''',
                      'id = input_id, value = list(nit = nit, linneg = as.character(fila$LinNegCod[[1]]))', text)
        text = text.replace('racafeForms::racafeModal(', 'racafeForms::racafeModalDetail(')
        text = text.replace('modal_size = "m",\n    modal_pre_fn = function(sel) .header_modal_compacto("75vh"),',
                            'modal_size = "subventana2",')
        text = replace_function(text, '.header_modal_compacto', '')
        text = remove_call(text, 'shinyjs::runjs("\n    if (!window.crmMigracionAccesoDirecto)')
        text = remove_call(text, 'shiny::observeEvent(shiny::getDefaultReactiveDomain()$input$crm_abrir_migracion,')
        text = text.replace('return(shiny::tags$div(class = "dropdown-item crm-header-vacio",\n                           "No hay clientes por revisar."))',
                            'return(racafeShiny::racafeListGroup(empty_message = "No hay clientes por revisar."))')
        text = text.replace('shiny::tagList(lapply(seq_len(nrow(datos)), function(i) {',
                            'do.call(racafeShiny::racafeListGroup, lapply(seq_len(nrow(datos)), function(i) {')
    elif name == '01_tabla_indicadores.R':
        text = replace_function(text, 'TablaIndicadoresUI', '''TablaIndicadoresUI <- function(id) {
  ns <- shiny::NS(id)
  racafeShiny::racafeOverlayLoader("indicadores_loader", ns = ns,
    visible = TRUE, image = "indicadores_loader.gif", triggers = "actualizar",
    racafeForms::racafeForm(size = "compact", variant = "plain",
      shiny::tags$div(id = ns("indicadores_contenido"),
        racafeForms::racafeDetailSection("Indicadores de mercado",
          gt::gt_output(ns("tabla_indicadores")), shiny::uiOutput(ns("ultima_actualizacion"))),
        racafeForms::racafeDetailSection("Posición",
          gt::gt_output(ns("tabla_disponible")), shiny::uiOutput(ns("pie_disponible")))),
      racafeForms::racafeFormFooter(racafeForms::racafeButton("actualizar", "Actualizar", icon = "sync",
        variant = "primary", size = "xxs", ns = ns, title = "Actualizar indicadores"))))
}''')
        text = replace_function(text, '.estilo_tabla', '''.estilo_tabla <- function(tabla) {
      racafeShiny::racafeCompactTable(tabla, backend = "gt", show_header = FALSE,
        font_size = 10, row_padding = 1, align = c("left", "right"))
    }''')
        text = text.replace('shinyjs::hide(id = session$ns("indicadores_loader"), asis = TRUE, anim = FALSE)',
                            'racafeShiny::racafeUpdateOverlayLoader(session, "indicadores_loader", FALSE)')
    elif name == '02_notificaciones_tareas.R':
        old = '''do.call(shiny::tagList, lapply(seq_along(actividades$textos), function(i) {
              racafeForms::racafeInputCheckbox(
                paste0("actividad_", i), actividades$textos[[i]],
                value = actividades$cumplidas[[i]], ns = ns
              )
            }))'''
        text = text.replace(old, '''racafeForms::racafeChecklist("actividades", actividades$textos,
              checked = actividades$cumplidas, ns = ns)''')
        text = text.replace('isTRUE(input[[paste0("actividad_", i)]])',
                            'as.character(i) %in% input$actividades')
        text = text.replace('racafeForms::racafeModal(', 'racafeForms::racafeModalDetail(')
        text = text.replace('racafeForms::racafeInfoItem("VENCIMIENTO", registro$FechaCumplimiento[[1]])',
          'racafeForms::racafeInfoItem("VENCIMIENTO", racafeForms::racafeDateLabel(registro$FechaCumplimiento[[1]]))')
        text = re.sub(r'''onclick = sprintf\(\s*"event.stopPropagation\(\);Shiny.setInputValue\('%s',%s,\{priority:'event'\}\);",\s*ns\("abrir"\), jsonlite::toJSON\(id, auto_unbox = TRUE\)\s*\)''',
                      'id = ns("abrir"), value = id', text)
        text = text.replace('shiny::tagList(lapply(seq_len(nrow(datos)), function(i) {',
                            'do.call(racafeShiny::racafeListGroup, lapply(seq_len(nrow(datos)), function(i) {')
        text = text.replace('''return(shiny::tags$div(
          class = "dropdown-item", "CRMNALNOTAS no tiene registros."
        ))''', '''return(racafeShiny::racafeListGroup(empty_message = "CRMNALNOTAS no tiene registros."))''')
    elif name == '04_migrar_cartera.R':
        text = text.replace('  shiny::tagList(\n    racafeForms::racafeForm(',
                            '  racafeShiny::racafeOverlayLoader("migracion_loader", ns = ns, triggers = "confirm_migracion",\n    racafeForms::racafeForm(', 1)
        text = text.replace('''      preloader <- racafeShiny::racafePreloader("calculando", "Migrando cartera...")
      waiter::waiter_show(html = preloader$html, color = preloader$color)
      on.exit(waiter::waiter_hide(), add = TRUE)''', '''      racafeShiny::racafeUpdateOverlayLoader(session, "migracion_loader", TRUE)
      on.exit(racafeShiny::racafeUpdateOverlayLoader(session, "migracion_loader", FALSE), add = TRUE)''')
    elif name == 'header.R':
        text = remove_call(text, 'tags$style(HTML(')
        text = remove_call(text, 'tags$script(HTML(')
        while 'htmltools::tagAppendAttributes(' in text:
            start = text.index('htmltools::tagAppendAttributes(')
            end = closing(text, text.index('(', start))
            inner = text.index('racafeModulos::MenuHeaderUI(', start)
            inner_end = closing(text, text.index('(', inner))
            call = text[inner:inner_end]
            if any(value in call for value in ['"MenuTareas"', '"MenuClientes"', '"MenuClientesAnt"']):
                call = re.sub(r'min_width = "\d+px"', 'min_width = "390px"', call)
                call = call[:-1] + ', button_size = "sm", max_height = "520px", keep_open_on_select = TRUE)'
            if '"MenuMigrarCartera"' in call: call = call[:-1] + ', direct = TRUE)'
            text = text[:start] + call + text[end:]
    return text


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    parser.add_argument('destination', type=Path)
    args = parser.parse_args()
    args.destination.mkdir(parents=True, exist_ok=True)
    for name in ['00_header.R', '01_tabla_indicadores.R', '02_notificaciones_tareas.R',
                 '03_form_persona.R', '04_migrar_cartera.R', 'header.R']:
        raw = (args.source / name).read_bytes()
        bom = raw.startswith(b'\xef\xbb\xbf')
        original = raw.decode('utf-8-sig')
        newline = '\r\n' if '\r\n' in original else '\n'
        normal = original.replace('\r\n', '\n')
        result = migrate(name, normal)
        (args.destination / name).write_bytes((b'\xef\xbb\xbf' if bom else b'') +
            result.replace('\n', newline).encode('utf-8'))
        patch = ''.join(difflib.unified_diff(normal.splitlines(keepends=True), result.splitlines(keepends=True),
            fromfile='a/' + name, tofile='b/' + name))
        patch_path = args.destination / (name + '.patch')
        if patch: patch_path.write_text(patch, encoding='utf-8')
        else: patch_path.unlink(missing_ok=True)
        print(name, ':', 'sin cambios (ya utiliza racafeForms)' if normal == result else 'UI migrada')


if __name__ == '__main__':
    main()
