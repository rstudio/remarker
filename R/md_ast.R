pandoc_markdown_format <- paste0(
  "markdown",
  "+backtick_code_blocks",
  "+fenced_code_attributes",
  "+yaml_metadata_block",
  "-hard_line_breaks"
)

#' @export
md_ast <- function(file = NULL, text = NULL, collapse_strings = TRUE) {
  if (!xor(is.null(file), is.null(text))) {
    stop("Must have exactly one of `file` or `text`.")
  }

  args <- c(
    "-f", pandoc_markdown_format,
    "-t", "json",
    if (collapse_strings) {
      c("--lua-filter", system.file("lua_filters/collapse_strings.lua", package = "remarker"))
    },
    if (!is.null(file)) c("-s", file)
  )

  js <- system2("pandoc", args = args, input = text, stdout = TRUE)

  json <- jsonlite::fromJSON(js, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  add_class(json,  "pandoc_ast")
}

#' @export
ast_md <- function(x, outfile = NULL) {
  stopifnot(inherits(x, "pandoc_ast"))
  stopifnot(is.character(outfile))

  tmpfile_json <- tempfile()
  json <- jsonlite::toJSON(unclass(x), auto_unbox = TRUE)
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


