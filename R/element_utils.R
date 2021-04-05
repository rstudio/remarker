#' Extract information from the Attr of an Element
#'
#' @param x A Block or Inline Element object which has an Attr child.
#'
#' @examples
#' h <- Header(
#'   attr = Attr(
#'     classes = Texts("x", "y"),
#'     attributes = TextText_s(list("a", "a_value"), list("b", "b_value"))
#'   )
#' )
#'
#' el_get_class(h)
#'
#' el_get_keyvals(h)
#'
#' @export
el_get_class <- function(x) {
  if (!inherits(x, c("Block", "Inline"))) {
    stop("x is not a Block or Inline object")
  }

  type <- x[["t"]]
  type_info <- ast_types[[type]]
  child_types <- type_info$children

  attr_child_idx <- which("Attr" == child_types)
  if (length(attr_child_idx) == 0) {
    stop("Block or Inline Element with type ", type,
         " does not have a child with type Attr.")
  }

  attr <- x$c[[attr_child_idx]]
  as.character(attr[[2]])
}


#' @rdname el_get_class
#' @export
el_get_keyvals <- function(x) {
 if (!inherits(x, c("Block", "Inline"))) {
    stop("x is not a Block or Inline object")
  }

  type <- x[["t"]]
  type_info <- ast_types[[type]]
  child_types <- type_info$children

  attr_child_idx <- which("Attr" == child_types)
  if (length(attr_child_idx) == 0) {
    stop("Block or Inline Element with type ", type,
         " does not have a child with type Attr.")
  }

  attr <- x$c[[attr_child_idx]]
  namify(attr[[3]])
}
