
indent_string <- function(branches, branch_label = NULL) {
  if (length(branches) == 0) {
    return("")
  }

  res <- character(length(branches))

  # Vertical bar (like "|" but taller)
  res[branches == "TRUE" ]       <- "\u2502   "
  res[branches == "FALSE"]       <- "    "

  if (is.null(branch_label)) {
    branch_label <- "\u2500\u2500"

  } else {
    branch_label <- as.character(branch_label)
    # Get string length here because crayon alters it.
    branch_label_length <- nchar(branch_label)

    branch_label <- crayon::reset(branch_label)

    if (branch_label_length == 1) {
      branch_label <- paste0("\u2500", branch_label)
    }
  }

  if (branches[length(branches)] == "TRUE") {
    # Vertical bar with line right (sort of like "|-")
    res[length(branches)] <- paste0("\u251c", branch_label, "\u2500")
  }
  # Lines up, right (sort of like a bottom left corner)
  res[branches == "LAST_CHILD" ] <- paste0("\u2514", branch_label, "\u2500")
  crayon::silver(paste(res, collapse = ""))
}

cat0 <- function(...) {
  cat(..., sep = "")
}

#' @export
print.Element <- function(x, ..., branches = character(0), branch_label = NULL) {
  classname <- class(x)[1]

  if (is.null(names(x))) {
    delims <- c("[", "]")
  } else {
    delims <- c("{", "}")
  }

  cat0(
    "\n", indent_string(branches, branch_label),
    crayon::silver(delims[1], classname, delims[2], sep = "")
  )

  # If this is the last child, then for any children, the shouldn't be a branch
  # at this level.
  if (length(branches) && branches[length(branches)] == "LAST_CHILD") {
    branches[length(branches)] <- "FALSE"
  }

  if (!is.null(x[["t"]])) {
    # Handle Block and Inline elements
    cat0(sprintf(" t:%s  ", crayon::magenta(x[["t"]])))

    content <- x[["c"]]

    if (is.null(content)) {
      # Do nothing

    } else if (length(content) == 1) {
      if (is.atomic(content)) {
        # Special case for strings and numbers: keep on same line
        if (is_string(content)) {
          cat0("c:", crayon::green('"', escape_str(content), '"', sep = ""))
        } else {
          cat0("c:", crayon::green(content))
        }

      } else {
        print(content, branches = c(branches, "LAST_CHILD"), branch_label = "c")
      }

    } else {
      print(content, branches = c(branches, "LAST_CHILD"), branch_label = "c")
    }

  } else if (is_unnamed(x)) {
    # Handle Blockss, Inliness, Blocks, Inlines, and element components like
    # Attr, QuoteType.
    branches[length(branches) + 1] <- "TRUE"
    for (i in seq_along(x)) {
      if (i == length(x)) branches[length(branches)] <- "LAST_CHILD"
      print(x[[i]], branches = branches, branch_label = i)
    }

  } else {
    stop("Unexpected data structure in printing.")
  }
}

# =====================================================================
# Override some base S3 methods in the context of this package.
# =====================================================================

print.character <- function(x, ..., branches = character(0), branch_label = NULL) {
  cat0(
    "\n", indent_string(branches, branch_label),
     crayon::green('"', escape_str(x), '"', sep = "")
  )
}

print.integer <- function(x, ..., branches = character(0), branch_label = NULL) {
  cat0("\n", indent_string(branches, branch_label), crayon::green(x))
}

print.numeric <- print.integer

print.NULL <- function(x, ..., branches = character(0), branch_label = NULL) {
  cat0("\n", indent_string(branches, branch_label), crayon::green("NULL"))
}

print.list <- print.Element


escape_str <- function(x) {
  gsub('"', '\\"', x, fixed = TRUE)
}
