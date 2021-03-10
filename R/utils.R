# @staticimports
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
