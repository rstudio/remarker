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
  # Put everything into a wellpanel
  x$children <- c(
    x$children[1],
    list(list(
      type = "codewrapper",
      id = "",
      class = character(0),
      attr = c(fn = "wellPanel"),
      children = c(
        list(list(
          type = "header",
          depth = 3,
          id = "",
          class = character(0),
          attr = named_chr(),
          children = x$children[2]
        )),
        x$children[-1:-2]
      )
    ))
  )

  # Extract first paragraph and turn it
  # str(x)
  # browser()

  x
})
