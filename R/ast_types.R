
# =====================================================================
# Block components
# =====================================================================
validate_blocks <- function(blocks) {
  if ( !(is.null(blocks) || is_unnamed_list(blocks)) ) {
    stop("`blocks` object must be NULL or an unnamed list")
  }

  for (x in blocks) {
    if (!inherits(x, "Block")) {
      stop('All elements in `blocks` must have class "Block".')
    }
  }
}

validate_blockss <- function(blockss) {
  if ( !(is.null(blockss) || is_unnamed_list(blockss)) ) {
    stop("`blockss` object must be NULL or an unnamed list")
  }

  for (x in blockss) {
    validate_blocks(x)
  }
}

#' @export
Block <- function(type, content = NULL) {
  if (is.null(content)) {
    add_class(list(t = type), "Block")

  } else {
    add_class(
      list(t = type, c = content),
      "Block"
    )
  }
}

#' @export
BlockQuote <- function(content) {
  validate_blocks(content)
  Block("BlockQuote", content)
}

#' @export
BulletList <- function(content) {
  validate_blockss(content)
  Block("BulletList", content)
}

#' @export
CodeBlock <- function(text, attr = Attr()) {
  stopifnot(is_string(text))
  validate_attr(attr)
  Block("CodeBlock", list(attr, text))
}

#' @export
DefinitionList <- function(content) {
  stop("Not yet implemented")
}

#' @export
Div <- function(content, attr = Attr()) {
  validate_attr(attr)
  validate_blocks(content)
  Block("Div", list(attr, content))
}

#' @export
Header <- function(level, content, attr = Attr()) {
  stopifnot(is_numeric(level))
  validate_inlines(content)
  validate_attr(attr)
  Block("Header", list(level, attr, content))
}

#' @export
HorizontalRule <- function() {
  Block("HorizontalRule")
}

#' @export
LineBlock <- function(content) {
  validate_inliness(content)
  Block("LineBlock", content)
}

#' @export
Null <- function() {
  Block("Null")
}

#' @export
OrderedList <- function(items, listAttributes) {
  validate_blockss(items)

  Block("OrderedList", list(listAttributes, items))
}

#' @export
Para <- function(content) {
  validate_inlines(content)
  Block("Para", content)
}

#' @export
Plain <- function(content) {
  validate_inlines(content)
  Block("Plain", content)
}

#' @export
RawBlock <- function(text, format) {
  stopifnot(is_string(text))
  stopifnot(is_string(format))
  Block("RawBlock", list(format, text))
}

#' @export
Table <- function(caption = Caption(), colspecs, head, bodies, foot, attr = Attr()) {
  validate_caption(caption)
  validate_colspecs(colspecs)
  # validate_tablehead(head)
  validate_attr(attr)
  stop("Not yet implemented")
}

# =====================================================================
# Inline components
# =====================================================================

validate_inlines <- function(inlines) {
  if ( !(is.null(inlines) || is_unnamed_list(inlines)) ) {
    stop("`inlines` object must be NULL or an unnamed list")
  }

  for (x in inlines) {
    if (!inherits(x, "Inline")) {
      stop('All elements in `inlines` must have class "Inline".')
    }
  }
}

validate_inliness <- function(inliness) {
  if ( !(is.null(inliness) || is_unnamed_list(inliness)) ) {
    stop("`inliness` object must be NULL or an unnamed list")
  }

  for (x in inliness) {
    validate_inlines(x)
  }
}


#' @export
Inline <- function(type, content = NULL) {
  if (is.null(content)) {
    add_class(list(t = type), "Inline")

  } else {
    add_class(
      list(t = type, c = content),
      "Inline"
    )
  }
}

#' @export
Cite <- function(content, citations) {
  validate_inlines(content)

  Inline("Cite", list(citations, content))
}

#' @export
Code <- function(text, attr = Attr()) {
  Inline("Code", list(attr, text))
}

#' @export
Emph <- function(content) {
  validate_inlines(content)
  Inline("Emph", content)
}

#' @export
Image <- function(caption = list(), src, title = "", attr = Attr()) {
  validate_attr(attr)
  validate_inlines(caption)

  Inline("Image", list(attr, caption, Target_(src, title)))
}

#' @export
LineBreak <- function() {
  Inline("LineBreak")
}

#' @export
Link <- function(content, target, title = "", attr = Attr()) {
  validate_attr(attr)
  validate_inlines(content)

  Inline("Link", list(attr, content, Target_(target, title)))
}

#' @export
Math <- function(mathtype, text) {
  stop("Not yet implemented")
}

#' @export
Note <- function(content) {
  validate_blocks(content)
  Inline("Note", content)
}

#' @export
Quoted <- function(quotetype, content) {
  validate_inlines(content)
  Inline("Quoted", list(QuoteType_(quotetype), content))
}

#' @export
QuoteType_ <- function(quotetype) {
  if (!is_string(quotetype) ||
      !(quotetype == "SingleQuote" ||  quotetype == "DoubleQuote"))
  {
    stop('`quotetype` must be "SingleQuote" or "DoubleQuote".')
  }

  add_class(
    list(t = quotetype),
    "QuoteType"
  )
}

#' @export
RawInline <- function(format, text) {
  stopifnot(is_string(text))
  stopifnot(is_string(format))
  Inline("RawInline", list(format, text))
}

#' @export
SmallCaps <- function(content) {
  validate_blocks(content)
  Inline("SmallCaps", content)
}

#' @export
SoftBreak <- function() {
  Inline("SoftBreak")
}

#' @export
Space <- function() {
  Inline("Space")
}

#' @export
Span <- function(content, attr = Attr()) {
  validate_attr(attr)
  validate_inlines(content)

  Inline("Span", list(attr, content))
}

#' @export
Str <- function(text) {
  stopifnot(is_string(text))
  Inline("Str", text)
}

#' @export
Strikeout <- function(content) {
  validate_inlines(content)
  Inline("Strikeout", content)
}

#' @export
Strong <- function(content) {
  validate_inlines(content)
  Inline("Strong", content)
}

#' @export
Subscript <- function(content) {
  validate_inlines(content)
  Inline("Subscript", content)
}

#' @export
Superscript <- function(content) {
  validate_inlines(content)
  Inline("Superscript", content)
}

#' @export
Underline <- function(content) {
  validate_inlines(content)
  Inline("Underline", content)
}


# =====================================================================
# Element components
# =====================================================================

#' @export
Attr <- function(identifier = "", classes = list(), attributes = list()) {
  stopifnot(is_string(identifier))
  # stopifnot(is_classes(classes))

  if (!is.null(names(attributes))) {
    attributes <- mapply(attributes, names(attributes),
      FUN = function(value, name) {
        list(name, value)
      },
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    )
  }

  add_class(
    list(identifier, classes, attributes),
    "Attr"
  )
}

validate_attr <- function(attr) {
  stopifnot(inherits(attr, "Attr"))
}

Caption_ <- function(long = NULL, short = list()) {
  # Currently not sure how to use block-level captions
  validate_blocks(long)
  validate_inlines(short)
  add_class(
    list(long, short),
    "Caption"
  )
}

validate_caption <- function(attr) {
  stopifnot(inherits(attr, "Caption"))
}



ColSpec_ <- function(alignment = Alignment_(), colwidth = ColWidth_()) {
  stopifnot(inherits(alignment, "Alignment"))
  stopifnot(inherits(colwidth, "ColWidth"))
  add_class(
    list(alignment, colwidth),
    "ColSpec"
  )
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

Alignment_ <- function(alignment = "AlignDefault") {
  stopifnot(alignment %in% c("AlignDefault", "AlignLeft", "AlignRight", "AlignCenter"))
  add_class(
    list(t = alignment),
    "Alignment"
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
    list(start, list(t = style), list(t = delimiter)),
    "ListAttributes"
  )
}


Target_ <- function(url, title) {
  stopifnot(is_string(url))
  stopifnot(is_string(title))
  add_class(
    list(url, title),
    "Target"
  )
}
