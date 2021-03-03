
indent_string <- function(branches) {
  if (length(branches) == 0) {
    return("")
  }

  res <- character(length(branches))

  # Vertical bar (like "|" but taller)
  res[branches == "TRUE" ]       <- "\U2502  "
  res[branches == "FALSE"]       <- "   "

  if (branches[length(branches)] == "TRUE") {
    # Vertical bar with line right (sort of like "|-")
    res[length(branches)] <- "\U251c──"
  }
  # Lines up, right (sort of like a bottom left corner)
  res[branches == "LAST_CHILD" ] <- "\U2514──"
  crayon::silver(paste(res, collapse = ""))
}

cat0 <- function(...) {
  cat(..., sep = "")
}

#' @export
print.Element <- function(x, ..., branches = character(0)) {
  classname <- class(x)[1]
  cat0("\n", indent_string(branches), crayon::silver("<", classname, ">", sep = ""))

  # If this is the last child, then for any children, the shouldn't be a branch
  # at this level.
  if (length(branches) && branches[length(branches)] == "LAST_CHILD") {
    branches[length(branches)] <- "FALSE"
  }

  if (!is.null(x[["t"]])) {
    # Handle Block and Inline elements
    cat0(sprintf(" t:%s  ", crayon::magenta(x[["t"]])))

    content <- x[["c"]]

    if (length(content) == 0) {
      # Do nothing

    } else if (length(content) == 1) {
      cat("c:")
      if (is.atomic(content)) {
        # Special case for strings and numbers: keep on same line
        cat0(crayon::green('"', escape_str(content), '"', sep = ""))
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
  cat0(
    "\n", indent_string(branches),
     crayon::green('"', escape_str(x), '"', sep = "")
  )
}

print.integer <- function(x, ..., branches = character(0)) {
  cat0("\n", indent_string(branches), crayon::green(x))
}

print.numeric <- print.character

print.list <- print.Element


escape_str <- function(x) {
  sub('"', '\\"', x, fixed = TRUE)
}
