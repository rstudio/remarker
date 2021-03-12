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
  if (! (is.null(value) || inherits(value, "Block")) ) {
    stop('`value` must be NULL or have class "Block"')
  }
  NextMethod()
}

#' @export
`[[<-.Blockss` <- function(x, name, value) {
  if (! (is.null(value) || inherits(value, "Blocks")) ) {
    stop('`value` must be NULL or have class "Blocks"')
  }
  NextMethod()
}


#' @export
BlockQuote <- function(content = Blocks()) {
  Element("BlockQuote", content)
}

#' @export
BulletList <- function(content = Blockss()) {
  Element("BulletList", content)
}

#' @export
CodeBlock <- function(text = "", attr = Attr()) {
  Element("CodeBlock", attr, text)
}

#' @export
DefinitionList <- function(content = InlineBlockss_s()) {
  Element("DefinitionList", content)
}

#' @export
Div <- function(content = Blocks(), attr = Attr()) {
  Element("Div", attr, content)
}

#' @export
Header <- function(level = 1L, content = Inlines(), attr = Attr()) {
  Element("Header", level, attr, content)
}

#' @export
HorizontalRule <- function() {
  Element("HorizontalRule")
}

#' @export
LineBlock <- function(content = Inliness()) {
  Element("LineBlock", content)
}

#' @export
Null <- function() {
  Element("Null")
}

#' @export
OrderedList <- function(items = Blockss(), listAttributes = ListAttributes()) {
  Element("OrderedList", listAttributes, items)
}

#' @export
Para <- function(content = Inlines()) {
  Element("Para", content)
}

#' @export
Plain <- function(content = Inlines()) {
  Element("Plain", content)
}

#' @export
RawBlock <- function(text = "", format = "") {
  Element("RawBlock", format, text)
}

#' @export
Table <- function(caption = Caption(), colspecs = ColSpecs(),
  head = TableHead(), bodies = TableBodys(), foot = TableFoot(),
  attr = Attr())
{
  Element("Table", attr, caption, colspecs, head, bodies, foot)
}
