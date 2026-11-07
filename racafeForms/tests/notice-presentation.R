library(racafeForms)
.html_notice <- function(...) htmltools::renderTags(racafeNoticeList(...))$html
stopifnot(grepl("rf-notice-list--info", .html_notice("Texto"), fixed = TRUE))
.custom_notice <- .html_notice(c("Primera línea", "Segunda línea"), title = "Metodología",
  font_size = 11, text_color = "#495057", accent_color = "#DA291C")
stopifnot(grepl("font-size:11px", .custom_notice, fixed = TRUE),
  grepl("color:#495057", .custom_notice, fixed = TRUE),
  grepl("--rf-notice-color:#DA291C", .custom_notice, fixed = TRUE),
  length(regmatches(.custom_notice, gregexpr("<li>", .custom_notice, fixed = TRUE))[[1]]) == 2L,
  grepl("&lt;script&gt;", .html_notice("<script>"), fixed = TRUE))
for (.invalid_size in list(0, -1, NA_real_, Inf, c(10, 11), "11")) {
  stopifnot(inherits(try(racafeNoticeList("Texto", font_size = .invalid_size), silent = TRUE), "try-error"))
}
stopifnot(inherits(try(racafeNoticeList("Texto", text_color = "red;background:blue"), silent = TRUE), "try-error"))
