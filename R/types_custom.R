# =====================================================================
# Custom types
# =====================================================================

#' @export
NestedSection <- function(header = Header(), content = Blocks()) {
  Element("NestedSection", header, content)
}

as_NestedSection <- function(x) {
  if (inherits(x, "NestedSection")) {
    return(x)
  }
  stop("Can't coerce x to a NestedSections object")
}
