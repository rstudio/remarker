#' Convert a remarker IR object to R code
#' @export
ir_r <- function(x, indent = 0) {
  if (is.character(x)) {
    return(paste0('"', escape_dbl_quotes(x), '"'))
  }

  switch(x$type,
    document = {
      sprintf('tagList(%s)', ir_r_children(x, indent + 2))
    },
    section = {
      sprintf('tagList(%s)', ir_r_children(x, indent + 2))
    },
    para = {
      sprintf('p(%s)', ir_r_children(x, indent + 2))
    },
    italic = {
      sprintf('tags$i(%s)', ir_r_children(x, indent + 2))
    },
    bold = {
      sprintf('tags$b(%s)', ir_r_children(x, indent + 2))
    },
    underline = {
      sprintf('tags$u(%s)', ir_r_children(x, indent + 2))
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
    horizontalrule = {
      "hr()"
    },
    bulletlist = {
      sprintf("tags$ul(%s)", ir_r_children(x, indent + 2))
    },
    item = {
      sprintf("tags$li(%s)", ir_r_children(x, indent + 2))
    },
    {
      ""
    }
  )
}


ir_r_children <- function(x, indent = 0) {
  res <- lapply(x$children, ir_r, indent = indent)

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
