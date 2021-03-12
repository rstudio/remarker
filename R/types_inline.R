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
  if (! (is.null(value) || inherits(value, "Inline")) ) {
    stop('`value` must be NULL or have class "Inline"')
  }
  NextMethod()
}

#' @export
`[[<-.Inliness` <- function(x, name, value) {
  if (! (is.null(value) || inherits(value, "Inlines")) ) {
    stop('`value` must be NULL or have class "Inlines"')
  }
  NextMethod()
}


#' @export
Cite <- function(content = Inlines(), citations = Citations()) {
  Element("Cite", citations, content)
}

#' @export
Code <- function(text = "", attr = Attr()) {
  Element("Code", attr, text)
}

#' @export
Emph <- function(content = Inlines()) {
  Element("Emph", content)
}

#' @export
Image <- function(caption = Inlines(), src = "", title = "", attr = Attr()) {
  Element("Image", attr, caption, Target(src, title))
}

#' @export
LineBreak <- function() {
  Element("LineBreak")
}

#' @export
Link <- function(content = Inlines(), target = "", title = "", attr = Attr()) {
  Element("Link", attr, content, Target(target, title))
}

#'
#' @export
Math_ <- function(mathtype = "DisplayMath", text = "") {
  # Renamed so as not to conflict with methods::Math
  stop("Not yet implemented")
}

#' @export
Note <- function(content = Blocks()) {
  Element("Note", content)
}

#' @export
Quoted <- function(quotetype = QuoteType(), content = Inlines()) {
  Element("Quoted", quotetype, content)
}

#' @export
RawInline <- function(format = "", text = "") {
  Element("RawInline", format, text)
}

#' @export
SmallCaps <- function(content = Inlines()) {
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
Span <- function(content = Inlines(), attr = Attr()) {
  Element("Span", attr, content)
}

#' @export
Str <- function(text = "") {
  Element("Str", text)
}

#' @export
Strikeout <- function(content = Inlines()) {
  Element("Strikeout", content)
}

#' @export
Strong <- function(content = Inlines()) {
  Element("Strong", content)
}

#' @export
Subscript <- function(content = Inlines()) {
  Element("Subscript", content)
}

#' @export
Superscript <- function(content = Inlines()) {
  Element("Superscript", content)
}

#' @export
Underline <- function(content = Inlines()) {
  Element("Underline", content)
}
