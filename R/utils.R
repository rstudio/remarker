unclass_recursive <- function(x) {
  x <- unclass(x)
  if (is.list(x)) lapply(x, unclass_recursive)
  else            x
}


named_list_val <- list(a = 1)[0]
# Returns an empty named list
named_list <- function() named_list_val

named_chr_val <- c(a = "c")[0]
# Returns an empty named character vector
named_chr <- function() named_chr_val


# Add a class to x. Faster than `structure(x, class = class)`, which is
# surprisingly slow.
add_class <- function(x, class) {
  class(x) <- class
  x
}

escape_dbl_quotes <- function(x) {
  gsub('"', '\\\\"', x)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


# =====================================================================
# purrr-like functions
# =====================================================================
# These functions provide a similar API to purrr, but some are significantly
# faster.
#
map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

map2 <- function(.x, .y, .f, ...) {
  mapply(.f, .x, .y, ..., SIMPLIFY = FALSE)
}

map_chr <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA_character_)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA_character_)
  }
}

map_lgl <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA)
  }
}

map_int <- function(.x, .f, ...) {
  if (is.character(.f)) {
    vapply(.x, `[[`, .f, ..., FUN.VALUE = NA_integer_)
  } else {
    vapply(.x, .f, ..., FUN.VALUE = NA_integer_)
  }
}
