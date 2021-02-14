
#' @export
ast_ir <- function(x) {
  stopifnot(inherits(x, "pandoc_ast"))

  if (!setequal(names(x), c("pandoc-api-version", "meta", "blocks"))) {
    stop("process_doc requires a Pandoc AST object.")
  }

  version <- paste(x$`pandoc-api-version`, collapse = ".")
  if (version != "1.22") {
    message("Pandoc API version is ", version,
            ". remarker understands version 1.22.")
  }

  process_type(x, "Pandoc")
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
        type = "italic",
        children = process_type(x, "[Inline]")
      )
    },
    Underline = {
      list(
        type = "underline",
        children = process_type(x, "[Inline]")
      )
    },
    Strong = {
      list(
        type = "bold",
        children = process_type(x, "[Inline]")
      )
    },
    Code = {
      c(
        type = "code",
        process_type(x[[1]], "Attr"),
        children = process_type(x[[2]], "Text")
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
      c(
        type = "codeblock",
        process_type(x[[1]], "Attr"),
        children = process_type(x[[2]], "Text")
      )
    },
    Header = {
      c(
        type  = "header",
        depth = process_type(x[[1]], "Int"),
        process_type(x[[2]], "Attr"),
        children  = process_type(x[[3]], "[Inline]")
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
        id    = x[[1]],
        class = as.character(x[[2]]),
        attr  = attr
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
        strs <- as.character(x[seq(start, length.out = len)], recursive = FALSE)
        paste(strs, collapse = "")
      } else {
        if (len != 1)
          stop("Sanity check failed: should be list (not a char vector) of length 1")
        x[start]
      }
    }
  )
}
