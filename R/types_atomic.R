

# =====================================================================
# Atomic types
# =====================================================================

as_Double <- function(x) {
  if (!is_numeric(x)) stop("x must be a number")
  x
}

as_Int <- function(x) {
  if (!is_numeric(x)) stop("x must be a number")
  as.integer(x)
}

as_ColSpan <- as_Int

as_RowSpan <- as_Int

as_RowHeadColumns <- as_Int


as_Text <- function(x) {
  if (!is_string(x)) stop("x must be a string")
  x
}

as_Format <- as_Text


#' @export
Texts <- function(...) {
  Element("Texts", ...)
}

# Converts input to list of single strings
# Input can be:
# 1. NULL
# 2. empty list
# 3. character vector
# 4. list of single strings (which is the output format)
as_Texts <- function(x) {
  if (length(x) == 0) {
    return(list())
  }
  if (is.character(x)) {
    return(as.list(x))
  }
  if (is_unnamed_list(x) && all(map_lgl(x, is_string))) {
    return(x)
  }

  stop("`x` cannot be coerced to a list of strings")
}


as_TextText <- function(x) {
  if (inherits(x, "TextText")) {
    return(x)
  }

  if (is.list(x) && length(x) == 2) {
    class(x) <- c("TextText", "Element")
    return(x)
  }

  stop("`x` cannot be coerced to a TextText")
}


#' @export
TextText_s <- function(...) {
  Element("TextText_s", ...)
}

# Input can be one of three forms:
# 1. named character vector
# 2. named list of single strings
# 3. unnamed list of unnamed lists, each containing two strings (which is the output format)
#
# This function will convert the input to the form (3).
as_TextText_s <- function(x) {
  # Check for (3)
  if (is_unnamed_list(x)) {
    results <- map_lgl(x, function(y) {
      is_unnamed_list(y) && length(y) == 2 &&
      is_string(y[[1]])  && is_string(y[[2]])
    })
    if (!all(results)) {
      stop("`x` cannot be coerced to an unnamed list of unnamed lists, each with two strings")
    }
    return(x)
  }

  # Handles cases (1) and (2)
  if (!is.null(names(x)) && (is.character(x) || is.list(x))) {
    x <- mapply(x, names(x),
      FUN = function(value, name) list(name, value),
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    )
    return(x)
  }

  stop("`x` cannot be coerced to an unnamed list of unnamed lists, each with two strings")
}
