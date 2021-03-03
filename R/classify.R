# Given the raw Pandoc AST object (directly converted from JSON), add S3
# classes. This function assumes that the input is well-formed (although without
# classes).
classify <- function(x, new_class = NULL) {
  class(x) <- new_class

  switch(new_class,
    Blockss = {
      x[] <- lapply(x, classify, "Blocks")
    },
    Blocks = {
      x[] <- lapply(x, classify, "Block")
    },
    Block = {
      type <- x[["t"]]
      child_types <- ast_types[[type]]$children

      if (length(child_types) == 0) {
        # Do nothing
      } else if (length(child_types) == 1) {
        x[["c"]] <- classify(x[["c"]], child_types)
      } else {
        x[["c"]] <- map2(x[["c"]], child_types, classify)
      }
    },
    Inliness = {
      x[] <- lapply(x, classify, "Inlines")
    },
    Inlines = {
      x[] <- lapply(x, classify, "Inline")
    },
    Inline = {
      type <- x[["t"]]
      child_types <- ast_types[[type]]$children

      if (length(child_types) == 0) {
        # Do nothing
      } else if (length(child_types) == 1) {
        x[["c"]] <- classify(x[["c"]], child_types)
      } else {
        x[["c"]] <- map2(x[["c"]], child_types, classify)
      }
    }
  )

  x
}
