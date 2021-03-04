# ======================================================================
# AST type definitions
# ======================================================================

ast_types <- list(
  # ====================================================================
  # Block elements
  # ====================================================================
  Blocks = list(
    structure = "list",
    category = "Blocks",
    children = list(list("Block"))
  ),
  Blockss = list(
    structure = "list",
    category = "Blockss",
    children = list(list("Blocks"))
  ),

  BlockQuote = list(
    structure = "tc",
    category = "Block",
    children = list("Blocks")
  ),
  BulletList = list(
    structure = "tc",
    category = "Block",
    children = list("Blockss")
  ),
  CodeBlock = list(
    structure = "tc",
    category = "Block",
    children = list("Attr", "Text")
  ),
  DefinitionList = list(
    structure = "tc",
    category = "Block",
    children = list("InlinesBlockss_s")
  ),
  Div = list(
    structure = "tc",
    category = "Block",
    children = list("Attr", "Blocks")
  ),
  Header = list(
    structure = "tc",
    category = "Block",
    children = list("Int", "Attr", "Inlines")
  ),
  HorizontalRule = list(
    structure = "tc",
    category = "Block",
    children = list()
  ),
  LineBlock = list(
    structure = "tc",
    category = "Block",
    children = list("Inliness")
  ),
  Null = list(
    structure = "tc",
    category = "Block",
    children = list()
  ),
  OrderedList = list(
    structure = "tc",
    category = "Block",
    children = list("ListAttributes", "Blockss")
  ),
  Para = list(
    structure = "tc",
    category = "Block",
    children = list("Inlines")
  ),
  Plain = list(
    structure = "tc",
    category = "Block",
    children = list("Inlines")
  ),
  RawBlock = list(
    structure = "tc",
    category = "Block",
    children = list("Format", "Text")
  ),
  Table = list(
    structure = "tc",
    category = "Block",
    children = list("Attr", "Caption", "ColSpecs", "TableHead", "TableBodys",
                    "TableFoot")
  ),

  # ====================================================================
  # Inline elements
  # ====================================================================
  Inlines = list(
    structure = "list",
    category = "Inlines",
    children = list(list("Inline"))
  ),
  Inliness = list(
    structure = "list",
    category = "Inliness",
    children = list(list("Inlines"))
  ),

  Cite = list(
    structure = "tc",
    category = "Inline",
    children = list("Citations", "Inlines")
  ),
  Code = list(
    structure = "tc",
    category = "Inline",
    children = list("Attr", "Text")
  ),
  Emph = list(
    structure = "tc",
    category = "Inline",
    children = list("Inlines")
  ),
  Image = list(
    structure = "tc",
    category = "Inline",
    children = list("Attr", "Inlines", "Target")
  ),
  LineBreak = list(
    structure = "tc",
    category = "Inline",
    children = list()
  ),
  Link = list(
    structure = "tc",
    category = "Inline",
    children = list("Attr", "Inlines", "Target")
  ),
  Math = list(
    structure = "tc",
    category = "Inline",
    children = list("MathType", "Text")
  ),
  Note = list(
    structure = "tc",
    category = "Inline",
    children = list("Blocks")
  ),
  Quoted = list(
    structure = "tc",
    category = "Inline",
    children = list("QuoteType", "Inlines")
  ),
  RawInline = list(
    structure = "tc",
    category = "Inline",
    children = list("Format", "Text")
  ),
  SmallCaps = list(
    structure = "tc",
    category = "Inline",
    children = list("Inlines")
  ),
  SoftBreak = list(
    structure = "tc",
    category = "Inline",
    children = list()
  ),
  Space = list(
    structure = "tc",
    category = "Inline",
    children = list()
  ),
  Span = list(
    structure = "tc",
    category = "Inline",
    children = list("Attr", "Inlines")
  ),
  Str = list(
    structure = "tc",
    category = "Inline",
    children = list("Text")
  ),
  Strikeout = list(
    structure = "tc",
    category = "Inline",
    children = list("Inlines")
  ),
  Strong = list(
    structure = "tc",
    category = "Inline",
    children = list("Inlines")
  ),
  Subscript = list(
    structure = "tc",
    category = "Inline",
    children = list("Inlines")
  ),

  # =====================================================================
  # Element components
  # =====================================================================
  Alignment = list(
    structure = "t_enum",
    category = "Alignment",
    children = list(),
    values = c("AlignLeft", "AlignRight", "AlignCenter", "AlignDefault")
  ),
  Attr = list(
    structure = "list",
    category = "Attr",
    children = list("Text", "Texts", "TextText_s")
  ),
  Caption = list(
    structure = "list",
    category = "Caption",
    # TODO: Blocks can be NULL?
    children = list("Blocks", "Inlines")
  ),
  Cell = list(
    structure = "list",
    category = "Cell",
    children = list("Attr", "Alignment", "RowSpan", "ColSpan", "Blocks")
  ),
  Cells = list(
    structure = "list",
    category = "Cells",
    children = list(list("Cell"))
  ),
  ColSpec = list(
    structure = "list",
    category = "ColSpec",
    children = list("Alignment", "ColWidth")
  ),
  ColSpecs = list(
    structure = "list",
    category = "ColSpecs",
    children = list(list("ColSpec"))
  ),
  ColWidth = list(
    structure = "tc",
    category = "ColWidth",
    # TODO: Optional string?
    children = list()
  ),
  InlinesBlockss = list(
    structure = "list",
    category = "InlinesBlockss",
    children = list("Inlines", "Blockss")
  ),
  InlinesBlockss_s = list(
    structure = "list",
    category = "InlinesBlockss_s",
    children = list(list("InlinesBlockss"))
  ),
  ListAttributes = list(
    structure = "list",
    category = "ListAttributes",
    children = list("Int", "ListNumberStyle", "ListNumberDelim")
  ),
  ListNumberStyle = list(
    structure = "t_enum",
    category = "ListNumberStyle",
    children = list(),
    values = c("DefaultStyle", "Example", "Decimal",
               "LowerRoman", "UpperRoman",
               "LowerAlpha", "UpperAlpha")
  ),
  ListNumberDelim = list(
    structure = "t_enum",
    category = "ListNumberDelim",
    children = list(),
    values = c("DefaultDelim", "Period", "OneParen", "TwoParens")
  ),
  QuoteType = list(
    structure = "t_enum",
    category = "QuoteType",
    children = list(),
    values = c("SingleQuote", "DoubleQuote")
  ),
  Row = list(
    structure = "list",
    category = "Row",
    children = list("Attr", "Cells")
  ),
  Rows = list(
    structure = "list",
    category = "Rows",
    children = list(list("Row"))
  ),
  TableBody = list(
    structure = "list",
    category = "TableBody",
    children = list("Attr", "RowHeadColumns", "Rows", "Rows")
  ),
  TableBodys = list(
    structure = "list",
    category = "TableBodys",
    children = list(list("TableBody"))
  ),
  TableFoot = list(
    structure = "list",
    category = "TableHead",
    children = list("Attr", "Rows")
  ),
  TableHead = list(
    structure = "list",
    category = "TableHead",
    children = list("Attr", "Rows")
  ),
  Target = list(
    structure = "list",
    category = "Target",
    children = list("Text", "Text")
  ),
  Texts = list(
    structure = "list",
    category = "Texts",
    children = list(list("Text"))
  ),
  TextText = list(
    structure = "list",
    category = "TextText",
    children = list("Text", "Text")
  ),
  TextText_s = list(
    structure = "list",
    category = "TextText_s",
    children = list(list("TextText"))
  ),

  # =====================================================================
  # Atomic types
  # =====================================================================
  Int = list(
    structure = "atomic",
    category = "atomic",
    children = list()
  ),
  ColSpan = list(
    # TODO: This is basically an alias for Int - make that work, as well
    structure = "atomic",
    category = "atomic",
    children = list()
  ),
  RowSpan = list(
    structure = "atomic",
    category = "atomic",
    children = list()
  ),
  RowHeadColumns = list(
    structure = "atomic",
    category = "atomic",
    children = list()
  ),
  Text = list(
    structure = "atomic",
    category = "atomic",
    children = list()
  )
)


#' @export
Element <- function(type, ...) {
  type_info <- ast_types[[type]]
  if (is.null(type_info)) {
    stop("Unknown AST type: ", type)
  }

  child_types <- type_info$children

  content <- list(...)
  content_length <- length(content)

  if (length(child_types) >= 1 && is.list(child_types[[1]])) {
    # Some types (like Blocks, Inlines) have a variable number of children, all
    # of the same type.
    child_type <- child_types[[1]][[1]]
    fn_name <- as.symbol(paste0("as_", child_type))

    for (i in seq_len(content_length)) {
      # Construct an expression like `content[[i]] <- as_Inlines(content[[i]])`
      expr <- substitute(content[[i]] <- fn(content[[i]]), list(fn = fn_name))
      eval(expr)
    }

  } else {
    # Most types have a specific number of children of various types.
    if (length(child_types) != content_length) {
      stop("Defined number of items does not match length of ...")
    }

    for (i in seq_len(content_length)) {
      # Construct an expression like `content[[i]] <- as_Inlines(content[[i]])`
      fn_name <- as.symbol(paste0("as_", child_types[[i]]))
      expr <- substitute(content[[i]] <- fn(content[[i]]), list(fn = fn_name))
      eval(expr)
    }
  }


  if (type_info$structure == "tc") {
    # Block, Inline, and a few others have 't' and 'c' fields.
    if (content_length == 0) {
      res <- list(t = type)
    } else if (length(content) == 1) {
      # If length 1, unwrap the content
      res <- list(t = type, c = content[[1]])
    } else {
      res <- list(t = type, c = content)
    }

  } else {
    # Most element component types like Attr are just unnamed lists; they don't
    # have 't' and 'c' field.
    res <- content
  }

  # Assign class like "Block", "Inline", "Attr"
  class(res) <- c(type_info$category, "Element")

  res
}



# =====================================================================
# Block components
# =====================================================================

#' @export
Blocks <- function(...) {
  Element("Blocks", ...)
}

#' @export
Blockss <- function(...) {
  Element("Blockss", ...)
}

# This can't actually do coercion because it doesn't know which Block type to
# coerce to, but it's the best we can do.
as_Block <- function(x) {
  if (inherits(x, "Block")) {
    return(x)
  }
  stop("Can't coerce `x` to Block")
}

#' @export
as_Blocks <- function(x) {
  if (inherits(x, "Blocks")) {
    return(x)
  }

  if (is.null(x)) {
    x <- list()
  }
  if (!is_unnamed_list(x)) {
    if (inherits(x, "Block")) {
      stop("`x` is a Block object. Did you mean to wrap it with `Blocks()`?")
    }
    stop("`x` must be NULL or an unnamed list")
  }

  do.call(Blocks, x)
}

#' @export
as_Blockss <- function(x) {
  if (inherits(x, "Blockss")) {
    return(x)
  }
  if (is.null(x)) {
    x <- list()
  }
  if (!is_unnamed_list(x)) {
    stop("`x` must be NULL or an unnamed list")
  }

  do.call(Blockss, x)
}


#' @export
`[[<-.Block` <- function(x, name, value) {
  if (name == "t" && !is_string(value)) {
    stop("Value assigned to `t` must be a string.")
  }
  NextMethod()
}

#' @export
`$<-.Block` <- `[[<-.Block`

#' @export
`[[<-.Blocks` <- function(x, name, value) {
  if (!inherits(value, "Block")) {
    stop('`value` must be an object of class "Block"')
  }
  NextMethod()
}

#' @export
`[[<-.Blockss` <- function(x, name, value) {
  if (!inherits(value, "Blocks")) {
    stop('`value` must be an object of class "Blocks"')
  }
  NextMethod()
}


#' @export
BlockQuote <- function(content) {
  Element("BlockQuote", content)
}

#' @export
BulletList <- function(content) {
  Element("BulletList", content)
}

#' @export
CodeBlock <- function(text, attr = Attr()) {
  Element("CodeBlock", attr, text)
}

#' @export
DefinitionList <- function(content) {
  stop("Not yet implemented")
}

#' @export
Div <- function(content, attr = Attr()) {
  Element("Div", attr, content)
}

#' @export
Header <- function(level, content, attr = Attr()) {
  Element("Header", level, attr, content)
}

#' @export
HorizontalRule <- function() {
  Element("HorizontalRule")
}

#' @export
LineBlock <- function(content) {
  Element("LineBlock", content)
}

#' @export
Null <- function() {
  Element("Null")
}

#' @export
OrderedList <- function(items, listAttributes) {
  Element("OrderedList", listAttributes, items)
}

#' @export
Para <- function(content) {
  Element("Para", content)
}

#' @export
Plain <- function(content) {
  Element("Plain", content)
}

#' @export
RawBlock <- function(text, format) {
  Element("RawBlock", format, text)
}

#' @export
Table <- function(caption = Caption(), colspecs, head, bodies, foot, attr = Attr()) {
  Element("Table", attr, caption, colspecs, head, bodies, foot)
}

# =====================================================================
# Inline components
# =====================================================================

#' @export
Inlines <- function(...) {
  Element("Inlines", ...)
}

#' @export
Inliness <- function(...) {
  Element("Inliness", ...)
}


# This can't actually do coercion because it doesn't know which Inline type to
# coerce to, but it's the best we can do.
as_Inline <- function(x) {
  if (inherits(x, "Inline")) {
    return(x)
  }
  stop("Can't coerce `x` to Inline")
}

#' @export
as_Inlines <- function(x) {
  if (inherits(x, "Inlines")) {
    return(x)
  }

  if (is.null(x)) {
    x <- list()
  }

  if (!is_unnamed_list(x)) {
    if (inherits(x, "Inline")) {
      stop("`x` is an Inline object. Did you mean to wrap it with `Inlines()`?")
    }
    stop("`x` must be NULL or an unnamed list")
  }

  do.call(Inlines, x)
}

#' @export
as_Inliness <- function(inliness) {
  if (inherits(x, "Inliness")) {
    return(x)
  }

  if (is.null(x)) {
    x <- list()
  }
  if (!is_unnamed_list(x)) {
    stop("`x` must be NULL or an unnamed list")
  }

  do.call(Inliness, x)
}


#' @export
`[[<-.Inline` <- function(x, name, value) {
  if (name == "t" && !is_string(value)) {
    stop("Value assigned to `t` must be a string.")
  }
  NextMethod()
}

#' @export
`$<-.Inline` <- `[[<-.Inline`

#' @export
`[[<-.Inlines` <- function(x, name, value) {
  if (!inherits(value, "Inline")) {
    stop('`value` must be an object of class "Inline"')
  }
  NextMethod()
}

#' @export
`[[<-.Inliness` <- function(x, name, value) {
  if (!inherits(value, "Inlines")) {
    stop('`value` must be an object of class "Inlines"')
  }
  NextMethod()
}


#' @export
Cite <- function(content, citations) {
  Element("Cite", citations, content)
}

#' @export
Code <- function(text, attr = Attr()) {
  Element("Code", attr, text)
}

#' @export
Emph <- function(content) {
  Element("Emph", content)
}

#' @export
Image <- function(caption = list(), src, title = "", attr = Attr()) {
  Element("Image", attr, caption, Target(src, title))
}

#' @export
LineBreak <- function() {
  Element("LineBreak")
}

#' @export
Link <- function(content, target, title = "", attr = Attr()) {
  Element("Link", attr, content, Target(target, title))
}

#' @export
Math <- function(mathtype, text) {
  stop("Not yet implemented")
}

#' @export
Note <- function(content) {
  Element("Note", content)
}

#' @export
Quoted <- function(quotetype, content) {
  Element("Quoted", quotetype, content)
}

#' @export
RawInline <- function(format, text) {
  Element("RawInline", format, text)
}

#' @export
SmallCaps <- function(content) {
  Element("SmallCaps", content)
}

#' @export
SoftBreak <- function() {
  Element("SoftBreak")
}

#' @export
Space <- function() {
  Element("Space")
}

#' @export
Span <- function(content, attr = Attr()) {
  Element("Span", attr, content)
}

#' @export
Str <- function(text) {
  Element("Str", text)
}

#' @export
Strikeout <- function(content) {
  Element("Strikeout", content)
}

#' @export
Strong <- function(content) {
  Element("Strong", content)
}

#' @export
Subscript <- function(content) {
  Element("Subscript", content)
}

#' @export
Superscript <- function(content) {
  Element("Superscript", content)
}

#' @export
Underline <- function(content) {
  Element("Underline", content)
}


# =====================================================================
# Element components
# =====================================================================

#' @export
Alignment <- function(alignment = c("AlignDefault", "AlignLeft", "AlignRight", "AlignCenter")) {
  alignment <- match.arg(alignment)
  Element("Alignment", alignment)
}

#' @export
Attr <- function(identifier = "", classes = list(), attributes = list()) {
  Element("Attr", identifier, classes, attributes)
}

# Accepts as input:
# * An Attr object
# * NULL
# * A list with 3 items which can be coerced to an Attr, by using do.call(Attr, x)
as_Attr <- function(x) {
  if (inherits(x, "Attr")) {
    return(x)
  }
  if (is.null(x)) {
    return(Attr())
  }
  if (is_unnamed_list(x) && length(x) == 3) {
    return(do.call(Attr, x))
  }

  stop("`x` must be NULL or an unnamed list of length 3.")
}

#' @export
Caption <- function(long = NULL, short = NULL) {
  Element("Caption", long, short)
}

as_Caption <- function(x) {
  if (inherits(x, "Caption")) {
    return(x)
  }
  if (!is_unnamed_list(x) || length(x) != 2) {
    stop("`x` cannot be coerced to Caption")
  }
  do.call(Caption, x)
}

#' @export
ColSpec <- function(alignment = Alignment(), colwidth = ColWidth()) {
  Element("ColSpec", alignment, colwidth)
}

as_ColSpec <- function(x) {
  if (inherits(x, "ColSpec")) {
    return(x)
  }

}

ColSpecs <- function(...) {
  x <- list(...)

}

validate_colspecs <- function(colspecs) {
  if ( !(is.null(colspecs) || is_unnamed_list(colspecs)) ) {
    stop("`colspecs` object must be NULL or an unnamed list")
  }

  for (x in colspecs) {
    if (!inherits(x, "ColSpec")) {
      stop('All elements in `colspecs` must have class "ColSpec".')
    }
  }
}


DefinitionList <- function(content) {
  Element("DefinitionList", content)
}

as_InlinesBlockss <- function(x) {
  if (inherits(x, "InlinesBlockss")) {
    return(x)
  }

  if (!is_unnamed_list(x) || length(x) != 2) {
    stop("`x` cannot be coerced to InlinesBlockss")
  }

  x[[1]] <- as_Inlines(x[[1]])
  x[[2]] <- as_Blockss(x[[2]])
  class(x) <- c("InlinesBlockss", "Element")
  x
}

as_InlinesBlockss_s <- function(x) {
  if (inherits(x, "InlinesBlockss_s")) {
    return(x)
  }

  if (!is_unnamed_list(x)) {
    stop("`x` cannot be coerced to InlinesBlockss_s")
  }

  x[] <- lapply(x, as_InlinesBlockss)
  class(x) <- c("InlinesBlockss_s", "Element")
  x
}


as_QuoteType <- function(quotetype) {
  if (!is_string(quotetype) ||
      !(quotetype == "SingleQuote" ||  quotetype == "DoubleQuote"))
  {
    stop('`quotetype` must be "SingleQuote" or "DoubleQuote".')
  }

  add_class(
    list(t = quotetype),
    c("QuoteType", "Element")
  )
}

ColWidth_ <- function(colwidth = NULL) {
  if (is.null(colwidth)) {
    res <- list(t = "ColWidthDefault")
  } else if (is_numeric(colwidth)) {
    res <- list(t = "ColWidth", c = colwidth)
  } else {
    stop("Invalid value of `colwidth`.")
  }

  add_class(res, "ColWidth")
}


#' @export
ListAttributes <- function(start, style, delimiter) {
  stopifnot(is_numeric(start))
  stopifnot(style %in% c("DefaultStyle", "Example", "Decimal", "LowerRoman",
                         "UpperRoman", "LowerAlpha", "UpperAlpha"))
  stopifnot(delimiter %in% c("DefaultDelim", "Period", "OneParen", "TwoParens"))

  add_class(
    list(as.integer(start), list(t = style), list(t = delimiter)),
    c("ListAttributes", "Element")
  )
}

as_ListAttributes <- function(x) {
  if (inherits(x, "ListAttributes")) {
    return(x)
  }

  do.call(ListAttributes, x)
}

Target <- function(url, title) {
  Element("Target", url, title)
}

# Accepts:
# 1. Target object
# 2. unnamed list of length 2, where each element is a string
as_Target <- function(x) {
  if (inherits(x, "Target")) {
    return(x)
  }
  if (is_unnamed_list(x) && length(x) == 2 &&
      is_string(x[[1]]) && is_string(x[[2]]) )
  {
    return(add_class(x, "Target"))
  }

  stop("`x` cannot be coerced to a Target")
}


as_Int <- function(x) {
  if (!is_numeric(x)) stop("x must be a number")
  as.integer(x)
}

as_Text <- function(x) {
  if (!is_string(x)) stop("x must be a string")
  x
}

as_Format <- as_Text

# Converts input to list of single strings
# Input can be:
# 1. NULL
# 2. empty list
# 3. character vector
# 4. list of single strings (which is the output format)
as_Texts <- function(x) {
  if (length(x) == 0) {
    return(list())
  }
  if (is.character(x)) {
    return(as.list(x))
  }
  if (is_unnamed_list(x) && all(map_lgl(x, is_string))) {
    return(x)
  }

  stop("`x` cannot be coerced to a list of strings")
}

# Input can be one of three forms:
# 1. named character vector
# 2. named list of single strings
# 3. unnamed list of unnamed lists, each containing two strings (which is the output format)
#
# This function will convert the input to the form (3).
as_TextText_s <- function(x) {
  # Check for (3)
  if (is_unnamed_list(x)) {
    results <- map_lgl(x, function(y) {
      is_unnamed_list(y) && length(y) == 2 &&
      is_string(y[[1]])  && is_string(y[[2]])
    })
    if (!all(results)) {
      stop("`x` cannot be coerced to an unnamed list of unnamed lists, each with two strings")
    }
    return(x)
  }

  # Handles cases (1) and (2)
  if (!is.null(names(x)) && (is.character(x) || is.list(x))) {
    x <- mapply(x, names(x),
      FUN = function(value, name) list(name, value),
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    )
    return(x)
  }

  stop("`x` cannot be coerced to an unnamed list of unnamed lists, each with two strings")
}
