# ======================================================================
# AST type definitions
# ======================================================================

# Derived from:
# https://hackage.haskell.org/package/pandoc-types-1.22/docs/Text-Pandoc-Definition.html
# https://pandoc.org/lua-filters.html

ast_types <- list(
  # ====================================================================
  # Block elements
  # ====================================================================
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

  # ====================================================================
  # Inline elements
  # ====================================================================
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
  Superscript = list(
    structure = "tc",
    category = "Inline",
    children = list("Inlines")
  ),
  Underline = list(
    structure = "tc",
    category = "Inline",
    children = list("Inlines")
  ),

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
    # This one doesn't fit into the type system as currently designed, since it
    # can have a variable structure:
    #   { t: "ColWidth", c: 0.5 }  or  { t: "ColWidthDefault"}
    # However, it does happen to work with classify().
    structure = "tc",
    category = "ColWidth",
    children = list("Double")
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
    category = "TableFoot",
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
  Double = list(
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
  Format = list(
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
  ),

  # =====================================================================
  # Custom types - not standard Pandoc AST
  # =====================================================================
  NestedSection = list(
    structure = "tc",
    category = "Block",
    children = list("Block", "Blocks")
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

  if (type_info$structure == "t_enum") {
    # If it's an enumerated type, the content must have one element.
    if (! content[[1]] %in% type_info$values) {
      stop(content[[1]], " is not in ", paste(type_info$values, collapse = ", "))
    }
    res <- list(t = content[[1]])
    class(res) <- c(type_info$category, "Element")
    return(res)
  }

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
