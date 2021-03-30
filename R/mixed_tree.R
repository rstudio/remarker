#' Convert an HTML tag object with Pandoc elements to a Pandoc Blocks object.
#'
#' @param x An html tag obect that also contains Pandoc Element objects.
#'
#' @export
mixed_to_rawblocks <- function(x) {
  elements <- fastmap::faststack()
  res <- extract_elements(x, elements)

  if (!is_installed("htmltools")) {
    stop("This function requires the htmltools package to be installed.")
  }
  res <- htmltools::renderTags(res)

  html <- strsplit(res$html, "__extracted_element__")[[1]]
  html_blocks <- lapply(html, RawBlock, format = "html")

  interleaved <- interleave(html_blocks, elements$as_list())
  interleaved <- lapply(interleaved, wrap_blocks)
  flatten_blocks_list(interleaved)
}

# Walk tree, marking Elements and saving them into `elements`, which must be a
# faststack object passed to this function. This returns the modified input with
# the string "__extracted_element__" in the place of each Element object found
# in the tree.
extract_elements <- function(x, elements) {
  if (!is.list(x)) {
    return(x)
  }
  if (inherits(x, "Element")) {
    elements$push(x)
    return(paste0("__extracted_element__"))
  }
  # If we got here, we're a shiny.tag, shiny.tag.list, or list.
  if (inherits(x, "shiny.tag")) {
    x$children[] <- lapply(x$children, extract_elements, elements)
  } else if (inherits(x, "shiny.tag.list") || is.list(x)) {
    x[] <- lapply(x, extract_elements, elements)
  }

  x
}

# Given an Element, wrap it into a Blocks object.
wrap_blocks <- function(x) {
  if (inherits(x, "Blocks")) {
    # No change
  } else if (inherits(x, "Block")) {
    x <- Blocks(x)
  } else if (inherits(x, "Inlines")) {
    x <- Blocks(Plain(x))
  } else if (inherits(x, "Inline")) {
    x <- Blocks(Plain(Inlines(x)))
  } else {
    stop(
      "Don't know how to handle object of class ",
      paste(class(x), collapse = ", ")
    )
  }

  x
}

# Given a list of Blocks elements, flatten into a single Blocks object.
flatten_blocks_list <- function(x) {
  for (i in seq_along(x)) {
    if (!inherits(x[[1]], "Blocks")) {
      stop('All items in x must have class "Blocks"')
    }
  }

  res <- unlist(x, recursive = FALSE)
  do.call(Blocks, res)
}
