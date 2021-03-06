#' @export
ast_filter <- function(x, ...) {
  if (!inherits(x, "Pandoc")) {
    stop("`x` must be an object of class Pandoc")
  }

  filters <- list(...)

  if (!is_all_named(filters) && !is_unnamed(filters)) {
    stop("ast_filter() arguments must either all be named functions, or unnamed lists of named functions.")
  }

  if (is_all_named(filters)) {
    filters <- list(filters)
  }

  # At this point, `filters` should have the structure:
  # list(
  #   list(Str = f, Inlines = f, Emph = f),
  #   list(Para = f, Str = f)
  # )

  for (filters_set in filters) {
    # `filters_set` is something like:
    #   list(Str = f, Inlines = f, Emph = f)

    filters_set <- categorize_filters(filters_set)
    # `filters_set` is something like:
    # list(
    #   Inline = list(Str = f, Emph = f),
    #   Inlines = list(Inlines = f),
    #   Block = list(), Blocks = list(), Meta = list(), Pandoc = list()
    # )

    for (i in seq_along(filters_set)) {
      category <- names(filters_set)[i]
      filters_set_category <- filters_set[[i]]
      # `category` is something like "Inline"
      # `filters_set_category` is something like list(Str = f, Emph = f)

      if (category == "Pandoc") {
        # Pandoc and Meta are special-cased; Pandoc only operates on the top-level
        # object, and Meta only operates on the $meta field, so no need to
        # walk the tree.
        if (length(filters_set_category) != 0) {
          x <- apply_filter(x, filters_set_category$Pandoc)
        }
      } else if (category == "Meta") {
        if (length(filters_set_category) != 0) {
          x$meta <- apply_filter(x$meta, filters_set_category$Meta)
        }
      } else {
        x <- ast_walk(x, filters_set_category, category)
      }
    }
  }

  x
}


# Walk the ast with each type of filter (Inline, Inlies
ast_walk <- function(x, filters, category) {
  if (length(filters) == 0) {
    return(x)
  }

  # "Block", "Inline", "Blocks", "Inlines", "Attr", etc.
  x_cat <- class(x)[1]

  if (x_cat == category) {
    if (category == "Block" || category == "Inline") {
      # "Str", "Para", etc.
      type <- x[["t"]]

      # Look for a filter for this specific type.
      filter_fn <- filters[[type]]

      # Fall back to "Inline" or "Block" filters, if more specific filter (like
      # "Str" or "Para") is not present.
      if (is.null(filter_fn)) {
        filter_fn <- filters[[category]]
      }

    } else if (category == "Blocks" || category == "Inlines") {
      filter_fn <- filters[[category]]
    }

    if (is.null(filter_fn)) {
      return(x)
    }

    # If we get here, we have a filter function for this element.
    x <- apply_filter(x, filter_fn)
  }

  # Recurse. We don't need to recurse into every possible kind of child list --
  # only the ones listed here.
  if (x_cat == "Inline" || x_cat == "Block") {
    if (!is.atomic(x$c)) {
      x$c <- ast_walk(x$c, filters, category)
    }

  } else if (x_cat %in% c("Inlines", "Blocks", "Inliness", "Blockss", "Pandoc", "Meta", "list")) {
    if (!is.atomic(x)) {
      x[] <- lapply(x, ast_walk, filters, category)
    }
  }

  x
}

# Call filter(x), and do some checking on the result.
# If filter(x) returns NULL, return the original object.
# If it returns an object, make sure the category matches.
apply_filter <- function(x, filter_fn) {
  old_category <- class(x)[1]

  res <- filter_fn(x)
  new_category <- class(res)[1]

  if (is.null(res)) {
    # Filter returned NULL; leave object unchanged.
    return(x)

  } else if (new_category == old_category) {
    # Filter returned a Block or Inline object, same as input.
    return(res)

  } else if (new_category == paste0(old_category, "s")) {
    # Filter returned a list of input objects, which we need to splice.
    stop("Splicing lists not implemented yet.")
  }

  stop('Category of returned object does not match. Original: "',
    old_category, '"  New: "', new_category, '"'
  )
}


# Given a flat list of filter functions like:
#   list(Inline = f, Para = f, Str = f, Pandoc = f, Inlines = f)
# return a list with structure like:
#   list(
#     Inline  = list(Inline = f, Str = f),
#     Inlines = list(Inlines = f),
#     Block   = list(Para = f)
#     Blocks  = list()
#     Meta    = list(),
#     Pandoc  = list(Pandoc = f)
#   )
#
# Note that within the Inline category, the functions are not sorted. The
# "Inline" function serves as a fallback if there's no specific filter for the
# type. That is handled by ast_filter().
categorize_filters <- function(filters) {
  if (inherits(filters, "filters_categorized")) {
    return(filters)
  }

  if (!is_all_named_list(filters)) {
    stop("`filters` must be a named list of filter functions.")
  }

  filter_categories <- c("Inline", "Inlines", "Block", "Blocks", "Meta", "Pandoc")

  category_map <- vapply(ast_types, `[[`, "category", FUN.VALUE = "")
  # Add a few types that aren't listed in ast_types
  category_map <- c(
    category_map,
    Inline = "Inline",
    Block  = "Block",
    Meta   = "Meta",
    Pandoc = "Pandoc"
  )

  filter_names <- names(filters)

  res <- named_list()
  for (category in filter_categories) {
    matching_categories <- category_map[filter_names] == category
    matching_categories <- names(matching_categories)[matching_categories]
    res[[category]] <- filters[matching_categories]
  }

  class(res) <- "filters_categorized"
  res
}
