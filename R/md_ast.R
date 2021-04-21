pandoc_markdown_format <- paste0(
  "markdown",
  "+backtick_code_blocks",
  "+fenced_code_attributes",
  "+yaml_metadata_block",
  "-hard_line_breaks",
  "+raw_html"
)

#' Read in Markdown content and return the Pandoc AST
#'
#' @param file A filename. Cannot be used with `text`.
#' @param text Markdown text, or a connection object. Cannot be used with
#'   `file`.
#' @param classify Add classes
#'
#' @seealso json_ast
#' @export
md_ast <- function(file = NULL, text = NULL, collapse_strings = TRUE, classify = TRUE) {
  if (!xor(is.null(file), is.null(text))) {
    stop("Must have exactly one of `file` or `text`.")
  }
  if (!is.null(file) && !is_string(file)) {
    stop("`file` must be a filename.")
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
  json_ast(text = json)
}

#' Read in Pandoc AST JSON and return the Pandoc AST as an R object
#'
#' @inheritParams md_ast
#' @param text Pandoc AST JSON text, or a connection object. Cannot be used with
#'   `file`.
#'
#' @seealso md_ast
#' @export
json_ast <- function(file = NULL, text = NULL, classify = TRUE) {
  if (!xor(is.null(file), is.null(text))) {
    stop("Must have exactly one of `file` or `text`.")
  }
  if (!is.null(file) && !is_string(file)) {
    stop("`file` must be a filename.")
  }

  if (!is.null(file)) {
    ast <- jsonlite::read_json(file, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  } else {
    ast <- jsonlite::parse_json(text, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  }

  if (classify) {
    ast$blocks <- classify(ast$blocks, "Blocks")
  }
  set_class(ast,  "Pandoc")
}


#' Given a Pandoc AST, write to a file
#'
#' @param x AST object.
#' @param outfile Output filename.
#' @export
ast_md <- function(x, outfile = NULL) {
  stopifnot(inherits(x, "Pandoc"))
  stopifnot(is.null(outfile) || is_string(outfile))

  tmpfile_json <- tempfile()
  ast_json(x, tmpfile_json)
  on.exit(file.remove(tmpfile_json))

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

#' @export
ast_json <- function(x, outfile = NULL) {
  if (is.null(outfile)) {
    jsonlite::toJSON(unclass_recursive(x), auto_unbox = TRUE)
  } else {
    jsonlite::write_json(unclass_recursive(x), outfile, auto_unbox = TRUE)
  }
}


#' @export
ast_html <- function(x, outfile = NULL) {
  stopifnot(inherits(x, "Pandoc"))
  stopifnot(is.null(outfile) || is_string(outfile))

  tmpfile_json <- tempfile()
  ast_json(x, tmpfile_json)
  on.exit(file.remove(tmpfile_json))

  txt <- system2(
    "pandoc",
    args = c(
      "-f", "json",
      "-t", "html",
      "--wrap=preserve",
      tmpfile_json,
      if (!is.null(outfile)) c("-o", outfile)
    ),
    stdout = if (is.null(outfile)) TRUE else ""
  )

  paste(txt, collapse = "")
}
