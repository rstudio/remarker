#' Convert a remarker IR object to R code
#' @export
ir_r <- function(x, indent = 0) {
  if (is.character(x)) {
    return(paste0('"', escape_dbl_quotes(x), '"'))
  }

  switch(x$type,
    section = {
      sprintf('tagList(%s)', ir_r_children(x, indent + 2))
    },
    para = {
      sprintf('p(%s)', ir_r_children(x, indent + 2))
    },
    header = {
      sprintf('h%s(id = "%s", %s)',
        x$depth,
        x$id,
        ir_r_children(x, indent + 2)
      )
    },
    code = {
      sprintf('code(%s)', ir_r_children(x, indent + 2))
    },
    codeblock = {
      sprintf("pre('%s')", escape_dbl_quotes(as.character(x$children)))
    },
    {
      ""
    }
  )
}

ir_r_children <- function(x, indent = 0) {
  res <- map(x$children, ir_r, indent = indent)

  if (length(res) > 1) {
    indent_txt <- paste0(rep(" ", indent), collapse = "")
    sep <- paste0(",\n",  indent_txt)
    res <- paste(res, collapse = sep)
    res <- paste0(
      "\n", indent_txt,
      res,
      "\n", paste0(rep(" ", max(0, indent - 2)), collapse = ""),
      sep = ""
    )
  }

  res
}
