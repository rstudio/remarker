
# Number of spaces per level of indenting
indent_step <- 3

indent_string <- function(level) {
  paste0(rep(" ", level * indent_step), collapse = "")
}

cat0 <- function(...) {
  cat(..., sep = "")
}

#' @export
print.Blocks <- function(x, ..., indent = 0) {
  classname <- class(x)
  cat0(sprintf("\n%s<%s>", indent_string(indent), classname))

  for (y in x) {
    print(y, indent = indent + 1)
  }
}

#' @export
print.Block <- function(x, ..., indent = 0) {
  classname <- class(x)
  cat0(sprintf("\n%s<%s> t:%-7s", indent_string(indent), classname, x[["t"]]))

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
}


#' @export
print.Inlines <- print.Blocks

#' @export
print.Inline <- print.Block


#' @export
print.QuoteType <- function(x, ..., indent = 0) {
  cat0("\n", indent_string(indent), "<QuoteType> t:", x[["t"]])
}


#' @export
print.Attr <- function(x, ..., indent = 0) {
  indent_str <- rep(" ", indent * 2)
  cat0("\n", indent_string(indent), "<Attr>:")
}


print.QuoteType <- function(x, ..., indent = 0) {
  classname <- class(x)
  cat0(sprintf("\n%s<%s>", indent_string(indent), classname))

  if (!is.null(x[["t"]])) {
    cat0(sprintf(" t:%-7s", x[["t"]]))

  } else if (is_unnamed(x)) {
    for (i in x) {
      print(i, indent = indent + 1)
    }
  }
}

print.Attr <- print.QuoteType


# Override some base S3 methods in the context of this package.

print.character <- function(x, ..., indent = 0) {
  cat0("\n", indent_string(indent), crayon::blue(x))
}

print.integer <- print.character
