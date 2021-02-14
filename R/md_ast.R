pandoc_markdown_format <- paste0(
  "markdown",
  "+backtick_code_blocks",
  "+fenced_code_attributes",
  "+yaml_metadata_block",
  "-hard_line_breaks"
)

#' @export
md_ast <- function(file) {
  js <- system2("pandoc",
    args = c(
      "-f", pandoc_markdown_format,
      "-t", "json",
      "-s", file
    ),
    stdout = TRUE
  )

  json <- jsonlite::fromJSON(js, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  add_class(json,  "pandoc_ast")
}

#' @export
ast_md <- function(x, outfile = NULL) {
  stopifnot(inherits(x, "pandoc_ast"))

  tmpfile_json <- tempfile()
  json <- toJSON(unclass(x), auto_unbox = TRUE)
  writeLines(json, tmpfile_json)
  system2(
    "pandoc",
    args = c(
      "-f", "json",
      "-t", pandoc_markdown_format,
      "--wrap=preserve",
      tmpfile_json,
      if (!is.null(outfile)) c("-o", outfile)
    )
  )
}
