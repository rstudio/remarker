
# =====================================================================
# Block components
# =====================================================================

#' @export
Blocks <- function(...) {
  x <- list(...)
  as_Blocks(x)
}

#' @export
Blockss <- function(...) {
  x <- list(...)
  as_Blockss(x)
}

block_types <- c(
  "BlockQuote",
  "BulletList",
  "CodeBlock",
  "DefinitionList",
  "Div",
  "Header",
  "HorizontalRule",
  "LineBlock",
  "Null",
  "OrderedList",
  "Para",
  "Plain",
  "RawBlock",
  "Table"
)

#' @export
as_Blocks <- function(x, classify_ = FALSE) {
  if (inherits(x, "Blocks")) {
    return(x)
  }

  if (is.null(x)) {
    x <- list()
  }
  if (!is_unnamed_list(x)) {
    stop("`x` must be NULL or an unnamed list")
  }

  for (i in seq_along(x)) {
    if (classify_) {
      type <- x[[i]][["t"]]
      if (type %in% block_types) {
        class(x[[i]]) <- "Block"
      } else {
        stop("Unknown block type: ", type)
      }

      switch(type,
        BlockQuote = {
          x[[i]][["c"]] <- as_Blocks(x[[i]][["c"]])
        },
        BulletList = {
          x[[i]][["c"]] <- as_Blocks(x[[i]][["c"]])
        },
        CodeBlock = {
        },
        DefinitionList = {
        },
        Div = {
        },
        Header = {
        },
        HorizontalRule = {
        },
        LineBlock = {
        },
        Null = {
        },
        OrderedList = {
        },
        Para = {
        },
        Plain = {
        },
        RawBlock = {
        },
        Table = {
        },
        {
          stop("Unknown Block type: ", type)
        }
      )

    } else {
      if (!inherits(x[[i]], "Block")) {
        stop('All elements in `x` must have class "Block".')
      }
    }
  }

  class(x) <- "Blocks"
  x
}

#' @export
as_Blockss <- function(x, classify_ = FALSE) {
  if (inherits(x, "Blockss")) {
    return(x)
  }
  if (is.null(x)) {
    x <- list()
  }
  if (!is_unnamed_list(x)) {
    stop("`x` must be NULL or an unnamed list")
  }

  # Slightly better error messages than lapply(); should be as fast or faster.
  for (i in seq_along(x)) {
    x[[i]] <- as_Blocks(x[[i]], classify_ = classify_)
  }
  class(x) <- "Blockss"
  x
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
  content <- as_Blocks(content)
  Block("BlockQuote", content)
}

#' @export
BulletList <- function(content) {
  content <- as_Blocks(content)
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
  content <- as_Blocks(content)
  Block("Div", list(attr, content))
}

#' @export
Header <- function(level, content, attr = Attr()) {
  stopifnot(is_numeric(level))
  content <- as_Inlines(content)
  validate_attr(attr)
  Block("Header", list(level, attr, content))
}

#' @export
HorizontalRule <- function() {
  Block("HorizontalRule")
}

#' @export
LineBlock <- function(content) {
  content <- as_Inliness(content)
  Block("LineBlock", content)
}

#' @export
Null <- function() {
  Block("Null")
}

#' @export
OrderedList <- function(items, listAttributes) {
  items <- as_Blocks(items)
  Block("OrderedList", list(listAttributes, items))
}

#' @export
Para <- function(content) {
  content <- as_Inlines(content)
  Block("Para", content)
}

#' @export
Plain <- function(content) {
  content <- as_Inlines(content)
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

#' @export
Inlines <- function(...) {
  x <- list(...)
  as_Inlines(x)
}

#' @export
Inliness <- function(...) {
  x <- list(...)
  as_Inliness(x)
}


inline_types <- c(
  "Cite",
  "Code",
  "Emph",
  "Image",
  "LineBreak",
  "Link",
  "Math",
  "Note",
  "Quoted",
  "RawInline",
  "SmallCaps",
  "SoftBreak",
  "Space",
  "Span",
  "Str",
  "Strikeout",
  "Strong",
  "Subscript",
  "Superscript",
  "Underline"
)


#' @export
as_Inlines <- function(x, classify_ = FALSE) {
  if (inherits(x, "Inlines")) {
    return(x)
  }

  if (is.null(x)) {
    x <- list()
  }
  if (!is_unnamed_list(x)) {
    stop("`x` must be NULL or an unnamed list")
  }

  for (i in seq_along(x)) {
    if (classify_) {
      if (x[[i]][["t"]] %in% inline_types) {
        class(x[[i]]) <- "Inline"
      } else {
        stop("Unknown inline type: ", x[[i]][["t"]])
      }

    } else {
      if (!inherits(x[[i]], "Inline")) {
        stop('All elements in `x` must have class "Inline".')
      }
    }
  }

  class(x) <- "Inlines"
  x
}

#' @export
as_Inliness <- function(inliness, classify_ = FALSE) {
  if (inherits(x, "Inliness")) {
    return(x)
  }

  if (is.null(x)) {
    x <- list()
  }
  if (!is_unnamed_list(x)) {
    stop("`x` must be NULL or an unnamed list")
  }

  # Slightly better error messages than lapply(); should be as fast or faster.
  for (i in seq_along(x)) {
    x[[i]] <- as_Inlines(x[[i]], classify_ = classify_)
  }

  class(x) <- "Inliness"
  x
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
  content <- as_Inlines(content)
  Inline("Cite", list(citations, content))
}

#' @export
Code <- function(text, attr = Attr()) {
  Inline("Code", list(attr, text))
}

#' @export
Emph <- function(content) {
  content <- as_Inlines(content)
  Inline("Emph", content)
}

#' @export
Image <- function(caption = list(), src, title = "", attr = Attr()) {
  validate_attr(attr)
  caption <- as_Inlines(caption)

  Inline("Image", list(attr, caption, Target_(src, title)))
}

#' @export
LineBreak <- function() {
  Inline("LineBreak")
}

#' @export
Link <- function(content, target, title = "", attr = Attr()) {
  validate_attr(attr)
  content <- as_Inlines(content)

  Inline("Link", list(attr, content, Target_(target, title)))
}

#' @export
Math <- function(mathtype, text) {
  stop("Not yet implemented")
}

#' @export
Note <- function(content) {
  content <- as_Blocks(content)
  Inline("Note", content)
}

#' @export
Quoted <- function(quotetype, content) {
  content <- as_Inlines(content)
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
  content <- as_Blocks(content)
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
  content <- as_Inlines(content)

  Inline("Span", list(attr, content))
}

#' @export
Str <- function(text) {
  stopifnot(is_string(text))
  Inline("Str", text)
}

#' @export
Strikeout <- function(content) {
  content <- as_Inlines(content)
  Inline("Strikeout", content)
}

#' @export
Strong <- function(content) {
  content <- as_Inlines(content)
  Inline("Strong", content)
}

#' @export
Subscript <- function(content) {
  content <- as_Inlines(content)
  Inline("Subscript", content)
}

#' @export
Superscript <- function(content) {
  content <- as_Inlines(content)
  Inline("Superscript", content)
}

#' @export
Underline <- function(content) {
  content <- as_Inlines(content)
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
  long <- as_Blocks(long)
  short <- as_Inlines(short)
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
