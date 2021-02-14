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
  jsonlite::fromJSON(js, simplifyDataFrame = FALSE, simplifyVector = FALSE)
}

#' @export
ast_md <- function(ast, outfile = NULL) {
  tmpfile_json <- tempfile()
  json <- toJSON(ast, auto_unbox = TRUE)
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

#' @export
process_doc <- function(x) {
  if (!setequal(names(x), c("pandoc-api-version", "meta", "blocks"))) {
    stop("process_doc requires a Pandoc AST object.")
  }

  version <- paste(x$`pandoc-api-version`, collapse = ".")
  if (version != "1.22") {
    message("Pandoc API version is ", version,
            ". remarker understands version 1.22.")
  }

  x <- process_type(x, "Pandoc")


}


#' @export
process_type <- function(x, type) {
  switch(type,

    Pandoc = {
      list(
        `pandoc-api-version` = x[["pandoc-api-version"]],
        # meta = process_type(x[["meta"]], "Meta"),
        blocks = process_type(x[["blocks"]], "[Block]")
      )
    },

    # ------------------------------
    # Inline elements
    # ------------------------------
    "[Inline]" = {
      res <- lapply(x, process_type, "Inline")
      flatten_strings(res)
    },
    Inline = {
      process_type(x[["c"]], x[["t"]])
    },
    Str = {
      x
    },
    Emph = {
      list(
        type = "emph",
        process_type(x, "[Inline]")
      )
    },
    Underline = {
      list(
        type = "underline",
        process_type(x, "[Inline]")
      )
    },
    Strong = {
      list(
        type = "strong",
        process_type(x, "[Inline]")
      )
    },
    Code = {
      list(
        attr = process_type(x[[1]], "Attr"),
        text = process_type(x[[2]], "Text")
      )
    },
    Quoted = {
      #TODO: Flatten
      quote_str    <- process_type(x[[1]], "QuoteType")
      inline_block <- process_type(x[[2]], "[Inline]")

      if (is.character(inline_block)) {
        paste0(quote_str, inline_block, quote_str)

      } else {
        list(
          quote_str,
          inline_block,
          quote_str
        )
      }
    },
    QuoteType = {
      if      (x[["t"]] == "DoubleQuote") '"'
      else if (x[["t"]] == "SingleQuote") "'"
      else stop("Unknown quote type:", x[["t"]])
    },
    Space = {
      " "
    },
    SoftBreak = {
      " "
    },
    LineBreak = {
      "\n"
    },

    # ------------------------------
    # Block elements
    # ------------------------------
    "[[Block]]" = {
      lapply(x, process_type, "[Block]")
    },
    "[Block]" = {
      lapply(x, process_type, "Block")
    },
    Block = {
      process_type(x[["c"]], x[["t"]])
    },
    Plain = {
      process_type(x, "[Inline]")
    },
    Para = {
      process_type(x, "[Inline]")
    },
    CodeBlock = {
      list(
        # type = "CodeBlock",
        attr = process_type(x[[1]], "Attr"),
        text = process_type(x[[2]], "Text")
      )
    },
    Header = {
      list(
        # type = "Header",
        depth = process_type(x[[1]], "Int"),
        attr  = process_type(x[[2]], "Attr"),
        text  = process_type(x[[3]], "[Inline]")
      )
    },
    BulletList = {
      process_type(x, "[[Block]]")
    },
    HorizontalRule = {
      x
    },

    # ------------------------------
    # Other types
    # ------------------------------
    Attr = {
      attr        <- vapply(x[[3]], `[[`, 2L, FUN.VALUE = "")
      names(attr) <- vapply(x[[3]], `[[`, 1L, FUN.VALUE = "")

      list(
        id = x[[1]],
        class = x[[2]],
        attr = attr
      )
    },
    Int = {
      as.integer(x)
    },
    Text = {
      x
    },
    {
      stop("Unknown type: ", type)
    }
  )
}


# Given a list of mixed strings and lists, concatenate adjacent strings into a
# single string wherever possible.
flatten_strings <- function(x) {
  runs <- rle(vapply(x, is.character, NA))
  starts <- cumsum(c(1L, runs$lengths)[seq_along(runs$lengths)])

  mapply(start = starts, len = runs$lengths, ischar = runs$values,
    FUN = function(start, len, ischar) {
      if (ischar) {
        strs <- unlist(x[seq(start, length.out = len)], recursive = FALSE)
        paste(strs, collapse = "")
      } else {
        if (len != 1)
          stop("Sanity check failed: should be list (not a char vector) of length 1")
        x[start]
      }
    }
  )
}
