library(racafeShiny)
.note_html <- htmltools::renderTags(racafeTableNote(c("Primera línea", "Segunda línea")))$html
stopifnot(grepl("font-size:11px", .note_html, fixed = TRUE), grepl("color:#495057", .note_html, fixed = TRUE),
  length(regmatches(.note_html, gregexpr("align-items:baseline", .note_html, fixed = TRUE))[[1]]) == 2L)
stopifnot(grepl("&lt;script&gt;", htmltools::renderTags(racafeTableNote("<script>"))$html, fixed = TRUE))
stopifnot(inherits(try(racafeTableNote("Texto", font_size = 0), silent = TRUE), "try-error"),
  inherits(try(racafeTableNote("Texto", color = "red;background:blue"), silent = TRUE), "try-error"),
  inherits(try(racafeTableNote(c("Uno", "Dos"), swatches = "#495057"), silent = TRUE), "try-error"))
.table_html <- htmltools::renderTags(racafeTableVancouver(data.frame(Producto = "Café", Sacos = 2),
  header_color = "#495057", header_width = 1.5, source_note = c("Nota externa", "Segunda línea")))$html
stopifnot(grepl("rsh-table-note", .table_html, fixed = TRUE),
  regexpr("</table>", .table_html, fixed = TRUE)[[1]] < regexpr("Nota externa", .table_html, fixed = TRUE)[[1]],
  !grepl('class="gt_sourcenote', .table_html, fixed = TRUE))
