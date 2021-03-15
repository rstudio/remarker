#' @export
nest_sections <- function(x) {
  if (inherits(x, "Pandoc")) {
    x$blocks <- nest_sections(x$blocks)
    return(x)
  }

  # TODO: Add type check -- maybe need to move implementation out to separate
  # function?

  # Find the depth of a Header; if not a Header, return 99
  depth <- function(obj) {
    if (obj$t == "Header") obj$c[[1]]
    else                   99L
  }

  res <- Blocks()

  # In x, the starting index of the current section
  section_start_idx <- 1L
  min_depth <- 99L

  for (i in seq_along(x)) {
    cur_depth <- depth(x[[i]])

    # Within a section, any higher order blocks
    if (min_depth == 99L) {
      if (cur_depth == min_depth) {
        res[[length(res) + 1L]] <- x[[i]]

      } else if (cur_depth < min_depth) {
        section_start_idx <- i
      }

    } else if (i == length(x) || (i < length(x) && depth(x[[i+1L]]) <= min_depth)) {
      # Enter into here when the next element is a header with a lower number
      # (i.e., a "higher-level" header), or when we hit the last block.

      # This is the end of this section.
      res[[length(res) + 1]] <- NestedSection(
        # Header block
        x[[section_start_idx]],
        # All the remaining blocks until the next section
        nest_sections(x[seq(section_start_idx+1, i)])
      )

      # Next block is the start of a new section
      section_start_idx <- i + 1
    }

    if (cur_depth < min_depth) {
      min_depth <- cur_depth
    }
  }

  res
}


#' @export
unnest_sections <- function(x) {
  if (inherits(x, "Pandoc")) {
    x$blocks <- unnest_sections(x$blocks)
    return(x)
  }

  if (inherits(x, "Blocks")) {
    if (length(x) == 0) return(x)

    res <- lapply(x, unnest_sections)
    res <- unlist(res, recursive = FALSE)
    res <- do.call(Blocks, res)
    return(res)
  }

  if (!inherits(x, "Block")) {
    stop("x must be a Blocks or Block object")
  }

  if (x$t != "NestedSection") {
    return(list(x))
  }
  # If we get here, it must be a NestedSection object.

  res <- Blocks()

  # Pull the Header block up
  res[[1]] <- x$c[[1]]

  # Unnest all the children
  children <- unnest_sections(x$c[[2]])
  res[1 + seq_along(children)] <- children

  res
}
