
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

  x <- process_type(x, "Pandoc")

  add_class(
    list(
      meta = x$meta,
      content = group_sections(x$blocks)
    ),
    "remarker_ir"
  )
}

#' @export
group_sections <- function(x) {
  # Add root node
  x <- c(
    list(list(
      type  = "header",
      depth = 0L,
      id    = "",
      class = character(),
      attr = named_chr(),
      children = list()
    )),
    x
  )

  # Step 1: group all non-section content in sections
  start_idx <- map_chr(x, "type", .default = "")
  start_idx <- which(start_idx == "header")

  stop_idx <- c(start_idx[-1] - 1, length(x))

  sections <- map2(start_idx, stop_idx, function(start, stop) {
    header_node <- x[[start]]
    list(
      type  = "section",
      depth = header_node[["depth"]],
      id    = header_node[["id"]],
      class = header_node[["class"]],
      attr  = header_node[["attr"]],
      children = x[seq(start, stop)]
    )
  })

  list(
    type = "document",
    children = sections
  )
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
      res <- flatten(res)
      join_strings(res)
    },
    Inline = {
      process_type(x[["c"]], x[["t"]])
    },
    Str = {
      x
    },
    Emph = {
      as.list(c(
        type = "italic",
        children = process_type(x, "[Inline]")
      ))
    },
    Underline = {
      as.list(c(
        type = "underline",
        children = process_type(x, "[Inline]")
      ))
    },
    Strong = {
      as.list(c(
        type = "bold",
        children = process_type(x, "[Inline]")
      ))
    },
    Code = {
      as.list(c(
        type = "code",
        process_type(x[[1]], "Attr"),
        children = process_type(x[[2]], "Text")
      ))
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
      list(
        type = "para",
        children = process_type(x, "[Inline]")
      )
    },
    CodeBlock = {
      as.list(c(
        type = "codeblock",
        process_type(x[[1]], "Attr"),
        children = process_type(x[[2]], "Text")
      ))
    },
    Header = {
      as.list(c(
        type  = "header",
        depth = process_type(x[[1]], "Int"),
        process_type(x[[2]], "Attr"),
        children  = process_type(x[[3]], "[Inline]")
      ))
    },
    BulletList = {
      items <- process_type(x, "[[Block]]")
      items <- lapply(items, function(y) {
        list(type = "item", children = y)
      })

      list(
        type = "bulletlist",
        children = items
      )
    },
    HorizontalRule = {
      list(
        type = "horizontalrule"
      )
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


flatten <- function(x) {
  # First descend into children and "hoist" them up if they're the right type.
  x <- lapply(x, function(y) {
    if (is.character(y)) return(y)

    # If this is an unnamed list, it can be flattened (it's like a JSON array)
    if (is.null(names(y))) return(flatten(y))

    # If we got here, this is a node with a `type`, and shouldn't be flattened.
    # Add an extra layer of list, so that when we unlist in the next step,
    # it will go back to original state.
    list(y)
  })

  unlist(x, recursive = FALSE)
}

# Given a list of mixed strings and lists, concatenate adjacent strings into a
# single string wherever possible.
join_strings <- function(x) {
  runs <- rle(vapply(x, is.character, NA))
  starts <- cumsum(c(1L, runs$lengths)[seq_along(runs$lengths)])

  mapply(start = starts, len = runs$lengths, ischar = runs$values,
    FUN = function(start, len, ischar) {
      if (ischar) {
        strs <- as.character(x[seq(start, length.out = len)], recursive = FALSE)
        paste(strs, collapse = "")
      } else {
        if (len != 1) {
          stop("Sanity check failed: should be list (not a char vector) of length 1")
        }
        x[start]
      }
    }
  )
}
