
# Number of spaces per level of indenting
indent_step <- 3

indent_string <- function(level) {
  paste0(rep(" ", level * indent_step), collapse = "")
}

cat0 <- function(...) {
  cat(..., sep = "")
}

#' @export
print.Element <- function(x, ..., indent = 0) {
  classname <- class(x)[1]
  cat0(sprintf("\n%s<%s>", indent_string(indent), classname))

  if (!is.null(x[["t"]])) {
    # Handle Block and Inline elements
    cat0(sprintf(" t:%-7s", x[["t"]]))

    content <- x[["c"]]

    if (length(content) == 0) {
      # Do nothing

    } else if (length(content) == 1) {
      cat("c:")
      if (is.atomic(content)) {
        # Special case for strings and numbers: keep on same line
        cat0(crayon::blue(content))
      } else {
        print(content, indent = indent + 1)
      }

    } else {
      cat("c:")
      for (i in content) {
        print(i, indent = indent + 1)
      }
    }

  } else if (is_unnamed(x)) {
    # Handle Blockss, Inliness, Blocks, Inlines, and element components like
    # Attr, QuoteType.
    for (i in x) {
      print(i, indent = indent + 1)
    }

  } else {
    stop("Unexpected data structure in printing.")
  }
}

# =====================================================================
# Override some base S3 methods in the context of this package.
# =====================================================================

print.character <- function(x, ..., indent = 0) {
  cat0("\n", indent_string(indent), crayon::blue(x))
}

print.integer <- print.character

print.numeric <- print.character

print.list <- print.Element
