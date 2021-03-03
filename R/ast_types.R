
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
      if (type %in% names(Block_types)) {
        class(x[[i]]) <- "Block"
      } else {
        stop("Unknown block type: ", type)
      }

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


Block_types <- list(
  BlockQuote = list(
    types = "Blocks"
  ),
  BulletList = list(
    types = "Blockss"
  ),
  CodeBlock = list(
    types = c("Attr", "Text")
  ),
  DefinitionList = list(
    # Not implemented yet
    types = NULL
  ),
  Div = list(
    types = c("Attr", "Blocks")
  ),
  Header = list(
    types = c("Int", "Attr", "Inlines")
  ),
  HorizontalRule = list(
    types = character(0)
  ),
  LineBlock = list(
    types = "Inliness"
  ),
  Null = list(
    types = character(0)
  ),
  OrderedList = list(
    types = c("ListAttributes", "Blockss")
  ),
  Para = list(
    types = "Inlines"
  ),
  Plain = list(
    types = "Inlines"
  ),
  RawBlock = list(
    types = c("Format", "Text")
  ),
  Table = list(
    # Not implemented yet
    types = NULL
  )
)



# TODO: auto-calculate coercion functions on load

#' @export
Block <- function(type, ...) {
  types <- Block_types[[type]]$types
  if (is.null(types)) {
    stop("Unknown Block type: ", type)
  }

  content <- list(...)
  content_length <- length(content)

  if (length(types) != content_length) {
    stop("Defined number of items does not match length of ...")
  }

  for (i in seq_len(content_length)) {
    # Construct a call like `as_Blocks(content[[i]])`
    fn_name <- as.symbol(paste0("as_", types[[i]]))
    call <- substitute(content[[i]] <- fn(content[[i]]), list(fn = fn_name))
    eval(call)
  }

  # Unwrap if length 1
  if (length(content) == 1) {
    content <- content[[1]]
  }

  if (content_length == 0) {
    res <- list(t = type)
  } else if (length(content) == 1) {
    # If length 1, unwrap the content
    res <- list(t = type, c = content[[1]])
  } else {
    res <- list(t = type, c = content)
  }

  class(res) <- "Block"
  res
}

#' @export
BlockQuote <- function(content) {
  Block("BlockQuote", content)
}

#' @export
BulletList <- function(content) {
  Block("BulletList", content)
}

#' @export
CodeBlock <- function(text, attr = Attr()) {
  Block("CodeBlock", attr, text)
}

#' @export
DefinitionList <- function(content) {
  stop("Not yet implemented")
}

#' @export
Div <- function(content, attr = Attr()) {
  Block("Div", attr, content)
}

#' @export
Header <- function(level, content, attr = Attr()) {
  Block("Header", level, attr, content)
}

#' @export
HorizontalRule <- function() {
  Block("HorizontalRule")
}

#' @export
LineBlock <- function(content) {
  Block("LineBlock", content)
}

#' @export
Null <- function() {
  Block("Null")
}

#' @export
OrderedList <- function(items, listAttributes) {
  Block("OrderedList", listAttributes, items)
}

#' @export
Para <- function(content) {
  Block("Para", content)
}

#' @export
Plain <- function(content) {
  Block("Plain", content)
}

#' @export
RawBlock <- function(text, format) {
  Block("RawBlock", format, text)
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
      if (x[[i]][["t"]] %in% names(Inline_types)) {
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

Inline_types <- list(
  Cite = list(
    types = c("Citations", "Inlines")
  ),
  Code = list(
    types = c("Attr", "Text")
  ),
  Emph = list(
    types = "Inlines"
  ),
  Image = list(
    types = c("Attr", "Inlines", "Target")
  ),
  LineBreak = list(
    types = character(0)
  ),
  Link = list(
    types = c("Attr", "Inlines", "Target")
  ),
  Math = list(
    types = c("MathType", "Text")
  ),
  Note = list(
    types = "Blocks"
  ),
  Quoted = list(
    types = c("QuoteType", "Inlines")
  ),
  RawInline = list(
    types = c("Format", "Text")
  ),
  SmallCaps = list(
    types = "Inlines"
  ),
  SoftBreak = list(
    types = character(0)
  ),
  Space = list(
    types = character(0)
  ),
  Span = list(
    types = c("Attr", "Inlines")
  ),
  Str = list(
    types = "Text"
  ),
  Strikeout = list(
    types = "Inlines"
  ),
  Strong = list(
    types = "Inlines"
  ),
  Subscript = list(
    types = "Inlines"
  )
)


#' @export
Inline <- function(type, ...) {
  types <- Inline_types[[type]]$types
  if (is.null(types)) {
    stop("Unknown Inline type: ", type)
  }

  content <- list(...)
  content_length <- length(content)

  if (length(types) != content_length) {
    stop("Defined number of items does not match length of ...")
  }

  for (i in seq_len(content_length)) {
    # Construct a call like `as_Inlines(content[[i]])`
    fn_name <- as.symbol(paste0("as_", types[[i]]))
    call <- substitute(content[[i]] <- fn(content[[i]]), list(fn = fn_name))
    eval(call)
  }

  # Unwrap if length 1
  if (length(content) == 1) {
    content <- content[[1]]
  }

  if (content_length == 0) {
    res <- list(t = type)
  } else if (length(content) == 1) {
    # If length 1, unwrap the content
    res <- list(t = type, c = content[[1]])
  } else {
    res <- list(t = type, c = content)
  }

  class(res) <- "Inline"
  res
}

#' @export
Cite <- function(content, citations) {
  Inline("Cite", citations, content)
}

#' @export
Code <- function(text, attr = Attr()) {
  Inline("Code", attr, text)
}

#' @export
Emph <- function(content) {
  Inline("Emph", content)
}

#' @export
Image <- function(caption = list(), src, title = "", attr = Attr()) {

  Inline("Image", attr, caption, Target(src, title))
}

#' @export
LineBreak <- function() {
  Inline("LineBreak")
}

#' @export
Link <- function(content, target, title = "", attr = Attr()) {
  Inline("Link", attr, content, Target(target, title))
}

#' @export
Math <- function(mathtype, text) {
  stop("Not yet implemented")
}

#' @export
Note <- function(content) {
  Inline("Note", content)
}

#' @export
Quoted <- function(quotetype, content) {
  Inline("Quoted", quotetype, content)
}

#' @export
RawInline <- function(format, text) {
  Inline("RawInline", format, text)
}

#' @export
SmallCaps <- function(content) {
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
  Inline("Span", attr, content)
}

#' @export
Str <- function(text) {
  Inline("Str", text)
}

#' @export
Strikeout <- function(content) {
  Inline("Strikeout", content)
}

#' @export
Strong <- function(content) {
  Inline("Strong", content)
}

#' @export
Subscript <- function(content) {
  Inline("Subscript", content)
}

#' @export
Superscript <- function(content) {
  Inline("Superscript", content)
}

#' @export
Underline <- function(content) {
  Inline("Underline", content)
}


# =====================================================================
# Element components
# =====================================================================

#' @export
Attr <- function(identifier = "", classes = list(), attributes = list()) {
  # TODO: generalize this code, like Blocks and Inlines
  identifier <- as_Text(identifier)
  classes    <- as_Texts(classes)
  attributes <- as_Text_Texts(attributes)

  add_class(
    list(identifier, classes, attributes),
    "Attr"
  )
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

as_QuoteType <- function(quotetype) {
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

Target <- function(url, title) {
  add_class(
    list(url, title),
    "Target"
  )
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
as_Text_Texts <- function(x) {
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
