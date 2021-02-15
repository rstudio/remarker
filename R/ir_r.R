#' Convert a remarker IR object to R code
#' @export
ir_r <- function(x, indent = 0, server = fastmap::faststack()) {
  if (inherits(x, "remarker_ir")) {
    return(ir_r(x$content))
  }

  if (is.character(x)) {
    return(paste0('"', escape_dbl_quotes(x), '"'))
  }

  switch(x$type,
    document = {
      res <- sprintf('fluidPage(%s)', ir_r_children(x, indent + 2, server))
      server_txt <- server$as_list()
      server_txt <- paste(server_txt, collapse = "\n")
      server_txt <- gsub("\n", "\n  ", server_txt)
      server_txt <- paste0(
        "function(input, output, session) {\n  ",
        server_txt,
        "\n}"
      )
      attr(res, "server") <- server_txt
      class(res) <- "remarker_r"
      res
    },
    section = {
      sprintf('tagList(%s)', ir_r_children(x, indent + 2, server))
    },
    para = {
      sprintf('p(%s)', ir_r_children(x, indent + 2, server))
    },
    italic = {
      sprintf('tags$i(%s)', ir_r_children(x, indent + 2, server))
    },
    bold = {
      sprintf('tags$b(%s)', ir_r_children(x, indent + 2, server))
    },
    underline = {
      sprintf('tags$u(%s)', ir_r_children(x, indent + 2, server))
    },
    header = {
      sprintf('h%s(id = "%s", %s)',
        x$depth,
        x$id,
        ir_r_children(x, indent + 2, server)
      )
    },
    code = {
      sprintf('code(%s)', ir_r_children(x, indent + 2, server))
    },
    codeblock = {
      if ("ui" %in% x$class) {
        sprintf("{%s}", as.character(x$children))

      } else if ("server" %in% x$class) {
        server$push(as.character(x$children))

      } else {
        sprintf("pre('%s')", escape_dbl_quotes(as.character(x$children)))
      }
    },
    horizontalrule = {
      "hr()"
    },
    bulletlist = {
      sprintf("tags$ul(%s)", ir_r_children(x, indent + 2, server))
    },
    item = {
      sprintf("tags$li(%s)", ir_r_children(x, indent + 2, server))
    },
    {
      ""
    }
  )
}


ir_r_children <- function(x, indent = 0, server) {
  res <- lapply(x$children, ir_r, indent = indent, server = server)

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


#' @export
print.remarker_r <- function(x, ...) {
  cat(sep = "",
    "====================\nUI code\n====================\n",
    x,
    "\n\n====================\nServer code\n====================\n",
    attr(x, "server", exact = TRUE)
  )
}


#' @export
app_r <- function(x) {
  ui <- eval(parse(text = x))
  server <- eval(parse(text = attr(x, "server", exact = TRUE)))
  shinyApp(ui, server)
}
