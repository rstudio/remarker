#' Create a Pandoc object
#'
#'
#' @export
Pandoc <- function(api_version = list(1L,22L), meta = Meta(), blocks = Blocks()) {
  x <- list(
    `pandoc-api-version` = api_version,
    meta = meta,
    blocks = blocks
  )

  class(x) <- "Pandoc"
  x
}


#' Return a Pandoc Meta object
#' @export
Meta <- function() {
  named_list()
}


#' Convert a Pandoc Element object to a full Pandoc object
#'
#' @param x a Pandoc Element object.
#'
#' @export
as_Pandoc <- function(x) {
  if (inherits(x, "Pandoc")) {
    return(x)
  } else if (inherits(x, "Blocks")) {
    # No change
  } else if (inherits(x, "Block")) {
    x <- Blocks(x)
  } else if (inherits(x, "Inlines")) {
    x <- Blocks(Para(x))
  } else if (inherits(x, "Inline")) {
    x <- Blocks(Para(Inlines(x)))
  } else {
    stop("Don't know how to handle object of class ", class(x))
  }

  Pandoc(blocks = x)
}
