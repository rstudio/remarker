
#' @export
md_ast <- function(file) {
  js <- system(paste0('pandoc -f commonmark_x -t json -s ', file), intern = TRUE)
  jsonlite::fromJSON(js, simplifyDataFrame = FALSE, simplifyVector = TRUE)
}


#' @export
process_type <- function(x, type) {
  switch(type,

    # ------------------------------
    # Inline elements
    # ------------------------------
    "[Inline]" = {
      res <- lapply(x, process_type, "Inline")
      runs <- rle(vapply(res, is.character, NA))
      starts <- cumsum(c(1L, runs$lengths)[seq_along(runs$lengths)])
      # browser()
      z <- mapply(start = starts, len = runs$lengths, ischar = runs$values,
        FUN = function(start, len, ischar) {
          if (ischar) {
            strs <- unlist(res[seq(start, length.out = len)], recursive = FALSE)
            paste(strs, collapse = "")
          } else {
            if (len != 1)
              stop("Sanity check failed: should be list (not a char vector) of length 1")
            res[start]
          }
        }
      )
    },
    Inline = {
      process_type(x[["c"]], x[["t"]])
    },
    Str = {
      x
    },
    Code = {
      list(
        attr = process_type(x[[1]], "Attr"),
        text = process_type(x[[2]], "Text")
      )
    },
    Space = {
      " "
    },

    # ------------------------------
    # Block elements
    # ------------------------------
    Block = {
      process_type(x[["c"]], x[["t"]])
    },
    CodeBlock = {
      list(
        attr = process_type(x[[1]], "Attr"),
        text = process_type(x[[2]], "Text")
      )
    },
    Header = {
      list(
        depth = process_type(x[[1]], "Int"),
        attr  = process_type(x[[2]], "Attr"),
        text  = process_type(x[[3]], "[Inline]")
      )
    },
    Plain = {
      process_type(x, "[Inline]")
    },
    Para = {
      process_type(x, "[Inline]")
    },

    # ------------------------------
    # Other types
    # ------------------------------
    Attr = {
      attr <- if (length(x[[3]]) == 0) named_list()
              else setNames(as.list(x[[3]][,2]), x[[3]][,1])
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
