
is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_numeric <- function(x, .length = 1) {
  is.numeric(x) && length(x) == .length &&  all(! is.na(x))
}

is_all_named <- function(x) {
  length(names(x)) == length(x) && all(names(x) != "")
}

# Tests whether an object is truly unnamed
is_unnamed <- function(x) {
  is.null(names(x))
}

is_unnamed_list <- function(x) {
  is.list(x) && is_unnamed(x)
}


is_named_list <- function(x) {
  is.list(x) && is_all_named(x)
}

is_classes <- function(x) {
  is.list(x) && all(vapply(x, is_string, logical(1)))
}
