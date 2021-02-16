#' Convert a remarker IR object to R code
#' @export
ir_r <- function(x, indent = 0, .data = NULL) {
  if (is.null(.data)) {
    .data <- list(
      ui     = fastmap::faststack(),
      server = fastmap::faststack(),
      layout = fastmap::faststack()
    )
  }

  if (inherits(x, "remarker_ir")) {
    return(ir_r(x$content, indent, .data))
  }

  if (is.character(x)) {
    return(paste0('"', escape_dbl_quotes(x), '"'))
  }

  switch(x$type,
    document = {
      res <- sprintf('fluidPage(%s)', ir_r_children(x, indent + 2, .data))

      ui_txt <- .data$ui$as_list()

      if (.data$layout$size() == 0) {
        ui_txt <- map_chr(ui_txt, "text")
        ui_txt <- paste(ui_txt, collapse = ",\n")
      } else {
        layout_txt <- paste(.data$layout$as_list(), collapse = ",\n")
        args <- map(ui_txt, "text")
        names(args) <- map_chr(ui_txt, "id", .default = "")
        # Drop any unnamed args
        args <- args[names(args) != ""]

        # A little hacky to use htmlTemplate, but it works for now.
        ui_txt <- do.call(
          htmltools::htmlTemplate,
          c(list(filename = NULL), args, text_ = layout_txt, document_ = FALSE)
        )
        ui_txt <- as.character(ui_txt)
      }

      server_txt <- .data$server$as_list()
      server_txt <- paste(server_txt, collapse = "\n")
      server_txt <- gsub("\n", "\n  ", server_txt)
      server_txt <- paste0(
        "function(input, output, session) {\n  ",
        server_txt,
        "\n}"
      )
      attr(ui_txt, "server") <- server_txt
      class(ui_txt) <- "remarker_r"
      ui_txt
    },
    section = {
      .data$ui$push(
        list(
          id = x$id,
          text = sprintf('tagList(%s)', ir_r_children(x, indent + 2, .data))
        )
      )
    },
    para = {
      sprintf('p(%s)', ir_r_children(x, indent + 2, .data))
    },
    italic = {
      sprintf('tags$i(%s)', ir_r_children(x, indent + 2, .data))
    },
    bold = {
      sprintf('tags$b(%s)', ir_r_children(x, indent + 2, .data))
    },
    underline = {
      sprintf('tags$u(%s)', ir_r_children(x, indent + 2, .data))
    },
    header = {
      sprintf('h%s(id = "%s", %s)',
        x$depth,
        x$id,
        ir_r_children(x, indent + 2, .data)
      )
    },
    code = {
      sprintf('code(%s)', ir_r_children(x, indent + 2, .data))
    },
    codeblock = {
      if ("ui" %in% x$class) {
        sprintf("{%s}", as.character(x$children))

      } else if ("server" %in% x$class) {
        .data$server$push(as.character(x$children))
        NULL

      } else if ("layout" %in% x$class) {
        .data$layout$push(as.character(x$children))
        NULL

      } else {
        sprintf("pre('%s')", escape_dbl_quotes(as.character(x$children)))
      }
    },
    horizontalrule = {
      "hr()"
    },
    bulletlist = {
      sprintf("tags$ul(%s)", ir_r_children(x, indent + 2, .data))
    },
    item = {
      sprintf("tags$li(%s)", ir_r_children(x, indent + 2, .data))
    },
    {
      ""
    }
  )
}


ir_r_children <- function(x, indent = 0, .data) {
  res <- lapply(x$children, ir_r, indent = indent, .data = .data)

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
