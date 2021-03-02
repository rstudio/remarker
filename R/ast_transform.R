#' @export
ast_walk <- function(x, f) {
  if (is.list(x)) {
    res <- f(x)
    if (!is.null(res)) {
      x <- res
    }
    x <- lapply(x, walk, f)
  }

  x
}

transforms <- new.env()

#' @export
transform_register <- function(name, f) {
  transforms[[name]] <- f
}
