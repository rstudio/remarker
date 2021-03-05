#' @export
ast_filter <- function(x, filters) {
  if (!is_named_list(filters)) {
    stop("`filters` must be a named list of filter functions.")
  }

  if (inherits(x, "Block") || inherits(x, "Inline")) {
    category <- class(x)[1]  # "Block" or "Inline"
    type <- x[["t"]]

    filter <- filters[[type]]
    if (!is.null(filter)) {
      res <- filter(x)
      new_category <- class(res)[1]

      if (is.null(res)) {
        # Filter returned NULL; leave object unchanged.

      } else if (new_category == category) {
        # Filter returned a Block or Inline object, same as before.
        x <- res

      } else if (new_category == paste0(category, "s")) {
        # Filter returned a list of input objects, which we need to splice.
        stop("Splicing lists not implemented yet.")

      } else {
        stop('Category of returned object does not match. Original: "',
          category, '"  New: "', new_category, '"'
        )
      }
    }
  }

  if (is.list(x)) {
    x[] <- lapply(x, ast_filter, filters)
  }
  x
}
