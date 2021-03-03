pandoc_markdown_format <- paste0(
  "markdown",
  "+backtick_code_blocks",
  "+fenced_code_attributes",
  "+yaml_metadata_block",
  "-hard_line_breaks"
)

#' Read in Markdown content and return the Pandoc AST
#'
#' @param file A filename. Cannot be used with `text`.
#' @param text Markdown text. Cannot be used with `file`.
#' @param classify Add classes
#' @export
md_ast <- function(file = NULL, text = NULL, collapse_strings = TRUE, classify = TRUE) {
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

  json <- system2("pandoc", args = args, input = text, stdout = TRUE)

  ast <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  if (classify) {
    ast$blocks <- as_Blocks(ast$blocks, classify_ = TRUE)
  }
  add_class(ast,  "pandoc_ast")
}

#' Given a Pandoc AST, write to a file
#'
#' @param x AST object.
#' @param outfile Output filename.
#' @export
ast_md <- function(x, outfile = NULL) {
  stopifnot(inherits(x, "pandoc_ast"))
  stopifnot(is.null(outfile) || is_string(outfile))

  tmpfile_json <- tempfile()
  json <- jsonlite::toJSON(unclass_recursive(x), auto_unbox = TRUE)
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
