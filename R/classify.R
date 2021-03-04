# Given the raw Pandoc AST object (directly converted from JSON), add S3
# classes. This function assumes that the input is well-formed (although without
# classes).
classify <- function(x, new_class = NULL) {

  if (is.null(x)) {
    # This can happen in Table captions?
    return(x)
  }

  if (is.list(new_class)) {
    if (length(new_class) != 1) {
      stop("Don't know how to handle this yet.")
    }
    if (!is_unnamed_list(x)) {
      stop("Don't know how to handle this yet.")
    }
    # browser()
    x[] <- lapply(x, classify, new_class[[1]])
  }


  # Block and Inline types are weird, because they're listed individually in
  # ast_types; there's a many-to-one mapping of entries in ast_types to Block
  # and Inline.
  if (new_class == "Block" || new_class == "Inline") {
    type <- x[["t"]]
    type_info <- ast_types[[type]]
    child_types <- type_info$children

    if (length(child_types) == 0) {
      # Do nothing
    } else if (length(child_types) == 1) {
      x[["c"]] <- classify(x[["c"]], child_types[[1]])
    } else {
      x[["c"]] <- map2(x[["c"]], child_types, classify)
    }

    class(x) <- c(new_class, "Element")
    return(x)
  }


  type_info <- ast_types[[new_class]]

  if (is.null(type_info)) {
    message("No type info for ", new_class)
    return(x)
  }

  # For atomic types like Int and Text, don't do anything to them because
  # they're don't assign them because they're just R numbers or strings.
  if (type_info$structure == "atomic") {
    return(x)
  }

  if (type_info$structure == "t_enum") {
    class(x) <- c(new_class, "Element")
    return(x)
  }


  # If we get here, we're working with elements other than Inline, Block, and
  # atomics.
  child_types <- type_info$children

  # If it has a tc structure, pull out the contents; we'll operate on them and
  # assign them back later. If it's a list structure, we're basically giving
  # another name to the object so that we can use the same code path.
  if (type_info$structure == "tc") {
    contents <- x[["c"]]
  } else {
    contents <- x
  }

  if (length(child_types) >= 1 && is.list(child_types[[1]])) {
    if (length(child_types) != 1) {
      stop("Don't know how to handle this yet.")
    }
    # This handles cases like Blocks and Inlines, where there's a variable
    # number of items, all of the same type.
    contents[] <- lapply(contents, classify, child_types[[1]][[1]])

  } else if (length(child_types) == 1 && is.atomic(contents)) {
    # Handles cases where c is an atomic value, like ColWidth.
    contents <- classify(contents, child_types[[1]])

  } else {
    # All other cases, like Attr, Caption, Row.
    contents[] <- map2(contents, child_types, classify)
  }

  if (type_info$structure == "tc") {
    x[["c"]] <- contents
  } else {
    x <- contents
  }


  class(x) <- c(new_class, "Element")
  x
}
