# @staticimports pkg:staticimports
#  is_installed
#  set_class
#  map map2 map_chr map_lgl map_int
#  named_list named_chr
#  %||%

unclass_recursive <- function(x) {
  x <- unclass(x)
  if (is.list(x)) lapply(x, unclass_recursive)
  else            x
}

escape_dbl_quotes <- function(x) {
  gsub('"', '\\\\"', x)
}

# Interleave two vectors. `b` can be the same length as `a`, or one shorter.
interleave <- function(a, b) {
  a_len <- length(a)
  b_len <- length(b)
  if (a_len != b_len && a_len != b_len + 1L) {
    stop("`b` must be the same length as `a`, or one shorter.")
  }

  # Make a copy of `a` with same type
  res <- a
  res[seq_len(a_len) * 2L - 1L] <- a
  res[seq_len(b_len) * 2L] <- b

  res
}

# Convert list/char vector with structure:
#   list(list("a", "a_val"), list("b", "b_val"))
# to:
#   c(a = "a_val", b = "b_val")
namify <- function(x) {
  vals        <- vapply(x, `[[`, 2, FUN.VALUE = NA_character_)
  names(vals) <- vapply(x, `[[`, 1, FUN.VALUE = NA_character_)
  vals
}
