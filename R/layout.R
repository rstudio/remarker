
transform_layout <- function(x) {
  layout_handler <- setdiff(x$class, "layout")

  for (class in x$class) {
    if (class %in% names(layouts)) {
      x <- layouts[[class]](x)
    }
  }

  x
}

layouts <- new.env()

#' @export
layout_register <- function(name, f) {
  layouts[[name]] <- f
}

layout_register("grid", function(x) {
  grid_obj <- grid_layout_from_md(x$children)

  content_ids <- unique(grid_obj)
  wrapper_ids <- paste0(content_ids, "_wrap")

  for (i in seq_along(content_ids)) {
    grid_obj[grid_obj == content_ids[i]] <- wrapper_ids[i]
  }

  css <- layout_to_css(grid_obj)

  divs_txt <- paste0('div(id = "', wrapper_ids, '", {{ ', content_ids , ' }})',
    collapse = ",\n")

  txt <- sprintf('
    tags$body(
      style = "display: grid;",
      tags$head(tags$style(HTML("%s"))),
      %s
    )',
    escape_dbl_quotes(css),
    divs_txt
  )
  x$children <- txt
  x
})
