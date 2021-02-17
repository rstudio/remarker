#' @export
ir_transform <- function(x) {
  if (!inherits(x, "remarker_ir")) {
    stop("ir_transform requires a remarker_ir object.")
  }
  x$content[] <- ir_transform_section(x$content)
  x
}

ir_transform_section <- function(x) {
  if (x$type == "document") {
    x$children <- lapply(x$children, ir_transform_section)

  } else if (x$type == "section") {
    for (class in x$class) {
      if (class %in% names(transforms)) {
        x <- transforms[[class]](x)
      }
    }

  } else {
    stop("ir_transform_section() can only be applied to an remarker_ir object or a section")
  }

  x
}



transforms <- new.env()

#' @export
transform_register <- function(name, f) {
  transforms[[name]] <- f
}

transform_register("card", function(x) {
  children <- x$children
  stopifnot(children[[1]]$type == "header")
  children[[1]]$depth <- 3

  # Put everything into a wellpanel
  x$children <- list(
    list(
      type = "codewrapper",
      id = "",
      class = character(0),
      attr = c(fn = "wellPanel"),
      children = children
    )
  )

  x
})
