
indent_string <- function(branches) {
  res <- character(length(branches))

  res[branches == "TRUE" ]       <- "│  "
  res[branches == "FALSE"]       <- "   "
  if (length(branches) && branches[length(branches)] == "TRUE") {
    res[length(branches)] <- "├──"
  }
  res[branches == "LAST_CHILD" ] <- "└──"
  paste(res, collapse = "")
}

cat0 <- function(...) {
  cat(..., sep = "")
}

#' @export
print.Element <- function(x, ..., branches = character(0)) {
  classname <- class(x)[1]
  cat0(sprintf("\n%s<%s>", indent_string(branches), classname))

  # If this is the last child, then for any children, the shouldn't be a branch
  # at this level.
  if (length(branches) && branches[length(branches)] == "LAST_CHILD") {
    branches[length(branches)] <- "FALSE"
  }

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
        print(content, branches = c(branches, "LAST_CHILD"))
      }

    } else {
      cat("c:")
      print(content, branches = c(branches, "LAST_CHILD"))
    }

  } else if (is_unnamed(x)) {
    # Handle Blockss, Inliness, Blocks, Inlines, and element components like
    # Attr, QuoteType.
    branches[length(branches) + 1] <- "TRUE"
    for (i in seq_along(x)) {
      if (i == length(x)) branches[length(branches)] <- "LAST_CHILD"
      print(x[[i]], branches = branches)
    }

  } else {
    stop("Unexpected data structure in printing.")
  }
}

# =====================================================================
# Override some base S3 methods in the context of this package.
# =====================================================================

print.character <- function(x, ..., branches = character(0)) {
  cat0("\n", indent_string(branches), crayon::blue(x))
}

print.integer <- print.character

print.numeric <- print.character

print.list <- print.Element
