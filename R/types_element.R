# =====================================================================
# Element components
# =====================================================================

#' @export
Alignment <- function(alignment = c("AlignDefault", "AlignLeft", "AlignRight", "AlignCenter")) {
  alignment <- match.arg(alignment)
  Element("Alignment", alignment)
}

as_Alignment <- function(x) {
  if (inherits(x, "Alignment")) {
    return(x)
  }

  if (!is_string(x) ||
      !(x %in% c("AlignDefault", "AlignLeft", "AlignRight", "AlignCenter")))
  {
    stop('`x` must be "AlignDefault", "AlignLeft", "AlignRight", or "AlignCenter".')
  }

  Alignment(x)
}


#' @export
Attr <- function(identifier = "", classes = Texts(), attributes = TextText_s()) {
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
Caption <- function(long = NULL, short = Inlines()) {
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
Cell <- function(attr = Attr(), alignment = Alignment(),
  colspan = 1L, rowspan = 1L, contents = Blocks())
{
  Element("Cell", attr, alignment, rowspan, colspan, contents)
}

#' @export
Cells <- function(...) {
  Element("Cells", ...)
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

#' @export
ColSpecs <- function(...) {
  Element("ColSpecs", ...)
}

as_ColSpecs <- function(x) {
  if (inherits(x, "ColSpecs")) {
    return(x)
  }
  stop("`x` cannot be coerced to a ColSpecs")
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


#' @export
ColWidth <- function(colwidth = NULL) {
  if (is.null(colwidth)) {
    # This is the one case where we can't call Element(), because the type spec
    # doesn't adequately describe the two different possible structures for
    # ColWidth objects.
    set_class(
      list(t = "ColWidthDefault"),
      c("ColWidth", "Element")
    )

  } else if (is_numeric(colwidth)) {
    Element("ColWidth", colwidth)

  } else {
    stop("Invalid value of `colwidth`.")
  }
}

as_ColWidth <- function(x) {
  if (inherits(x, "ColWidth")) {
    return(x)
  }

  ColWidth(x)
}

#' @export
InlinesBlockss <- function(inlines = Inlines(), blockss = Blockss()) {
  Element("InlinesBlockss", inlines, blockss)
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

#' @export
InlinesBlockss_s <- function(...) {
  Element("InlinesBlockss_s", ...)
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

# TODO: Add default enum values to args
#' @export
QuoteType <- function(quotetype = "SingleQuote") {
  Element("QuoteType", quotetype)
}

as_QuoteType <- function(x) {
  if (inherits(x, "QuoteType")) {
    return(x)
  }

  if (!is_string(x) ||
      !(x == "SingleQuote" ||  x == "DoubleQuote"))
  {
    stop('`quotetype` must be "SingleQuote" or "DoubleQuote".')
  }

  QuoteType(x)
}

#' @export
ListAttributes <- function(start = 1L, style = "DefaultStyle",
  delimiter = "DefaultDelim")
{
  stopifnot(is_numeric(start))
  stopifnot(style %in% c("DefaultStyle", "Example", "Decimal", "LowerRoman",
                         "UpperRoman", "LowerAlpha", "UpperAlpha"))
  stopifnot(delimiter %in% c("DefaultDelim", "Period", "OneParen", "TwoParens"))

  set_class(
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

#' @export
Row <- function(cells = Cells(), attr = Attr()) {
  Element("Row", attr, cells)
}

as_Row <- function(x) {
  if (inherits(x, "Row")) {
    return(x)
  }
  stop("`x` cannot be coerced to a Row")
}


#' @export
Rows <- function(...) {
  Element("Rows", ...)
}

as_Rows <- function(x) {
  if (inherits(x, "Rows")) {
    return(x)
  }
  stop("`x` cannot be coerced to a Rows")
}

#' @export
TableBody <- function(rowheadcolumns = 1L, head = Rows(), body = Rows(),
  attr = Attr())
{
  Element("TableBody", attr, rowheadcolumns, head, body)
}

#' @export
TableBodys <- function(...) {
  Element("TableBodys", ...)
}

as_TableBody <- function(x) {
  if (inherits(x, "TableBody")) {
    return(x)
  }
  stop("`x` cannot be coerced to a TableBody")
}


#' @export
TableBodys <- function(...) {
  Element("TableBodys", ...)
}

as_TableBodys <- function(x) {
  if (inherits(x, "TableBodys")) {
    return(x)
  }
  stop("`x` cannot be coerced to a TableBodys")
}


#' @export
TableHead <- function(rows = Rows(), attr = Attr()) {
  Element("TableHead", attr, rows)
}

as_TableHead <- function(x) {
  if (inherits(x, "TableHead")) {
    return(x)
  }
  stop("`x` cannot be coerced to a TableHead")
}


#' @export
TableFoot <- function(rows = Rows(), attr = Attr()) {
  Element("TableFoot", attr, rows)
}

as_TableFoot <- function(x) {
  if (inherits(x, "TableFoot")) {
    return(x)
  }
  stop("`x` cannot be coerced to a TableFoot")
}


#' @export
Target <- function(url = "", title = "") {
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
    return(set_class(x, "Target"))
  }

  stop("`x` cannot be coerced to a Target")
}
