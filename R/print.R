
#' @export
print.Blocks <- function(x, ..., indent = 0) {
  indent_str <- rep(" ", indent * 2)
  cat(indent_str, "<Blocks>\n", sep = "")
  for (y in x) {
    print(y, indent = indent + 1)
  }
}

#' @export
print.Block <- function(x, ..., indent = 0) {
  indent_str <- rep(" ", indent * 2)

  cat(indent_str, "<Block> t:", x[["t"]], sep = "")

  switch(x[["t"]],
    BlockQuote = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    BulletList = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    CodeBlock = {
      cat("\n")
    },
    DefinitionList = {
      cat("\n")
    },
    Div = {
      cat("\n")
    },
    Header = {
      cat(" c:\n")
      cat(indent_str, "  level: ", x[["c"]][[1]], "\n",
          indent_str, "  attr\n",
          sep = "")
      print(x[["c"]][[3]], indent = indent + 1)
    },
    HorizontalRule = {
      cat("\n")
    },
    LineBlock = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    Null = {
      cat("\n")
    },
    OrderedList = {
      cat("\n")
    },
    Para = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    Plain = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    RawBlock = {
      cat("\n")
    },
    Table = {
      cat("\n")
    },
    {
      stop("Unknown Block type: ", x[["t"]])
    }
  )

}



#' @export
print.Inlines <- function(x, ..., indent = 0) {
  indent_str <- rep(" ", indent * 2)
  cat(indent_str, "<Inlines>\n", sep = "")
  for (y in x) {
    print(y, indent = indent + 1)
  }
}

#' @export
print.Inline <- function(x, ..., indent = 0) {
  indent_str <- rep(" ", indent * 2)

  cat(indent_str, "<Inline> t:", x[["t"]], sep = "")

  switch(x[["t"]],
    Cite = {
      cat("\n")
    },
    Code = {
      # TODO: attr
      cat(' c:"', x[["c"]][[2]], '"\n', sep = "")
    },
    Emph = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    Image = {
      cat("\n")
    },
    LineBreak = {
      cat("\n")
    },
    Link = {
      cat("\n")
    },
    Math = {
      cat("\n")
    },
    Note = {
      cat("\n")
    },
    Quoted = {
      cat("\n")
    },
    RawInline = {
      cat("\n")
    },
    SmallCaps = {
      cat("\n")
    },
    SoftBreak = {
      cat("\n")
    },
    Space = {
      cat("\n")
    },
    Span = {
      cat("\n")
    },
    Str = {
      cat(' c:"', x[["c"]], '"\n', sep = "")
    },
    Strikeout = {
      cat("\n")
    },
    Strong = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    Subscript = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    Superscript = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    Underline = {
      cat(" c:\n")
      print(x[["c"]], indent = indent + 1)
    },
    {
      stop("Unknown Inline type: ", x[["t"]])
    }
  )

}
