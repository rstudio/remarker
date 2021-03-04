# Given the raw Pandoc AST object (directly converted from JSON), add S3
# classes. This function assumes that the input is well-formed (although without
# classes).
classify <- function(x, new_class = NULL) {

  # Exceptions for Int and Text classes; don't assign them because they're
  # just R numbers or strings.
  if ((new_class == "Int" || new_class == "Text") ) {
    return(x)
  }

  class(x) <- c(new_class, "Element")

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


  if (new_class == "Block" || new_class == "Inline") {
    # Block and Inline types are weird, because they're listed individually in
    # ast_types; there's a many-to-one mapping of entries in ast_types to Block
    # and Inline.
    type <- x[["t"]]
    child_types <- ast_types[[type]]$children

    if (length(child_types) == 0) {
      # Do nothing
    } else if (length(child_types) == 1) {
      x[["c"]] <- classify(x[["c"]], child_types[[1]])
    } else {
      x[["c"]] <- map2(x[["c"]], child_types, classify)
    }

  } else {
    type_info <- ast_types[[new_class]]
    if (is.null(type_info)) {
      message("No type info for ", new_class)
      return(x)
    }
    child_types <- type_info$children

    if (length(child_types) >= 1) {
      if (type_info$structure == "tc") {
        stop("Don't know how to handle this yet.")
      }

      if (is.list(child_types[[1]])) {
        if (length(child_types) != 1) {
          stop("Don't know how to handle this yet.")
        }
        # This handles cases like Blocks and Inlines, where there's a variable
        # number of items, all of the same type.
        x[] <- lapply(x, classify, child_types[[1]][[1]])

      } else {
        x[] <- map2(x, child_types, classify)
      }

    } else {
      stop("Don't know how to handle this yet.")
    }
  }

  x
}
