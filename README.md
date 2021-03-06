
<!-- README.md is generated from README.Rmd. Please edit that file -->

# remarker

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/remarker/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/remarker/actions)
<!-- badges: end -->

**Remarker** is an R package which makes it easy to use Pandoc to read
in Markdown (and other) files, as the Pandoc AST, apply transformations
(also known as filters) on them, and send the AST back to Pandoc for
conversion to Markdown, HTML, or other formats. In short, it is an
implementation of [Pandoc filters](https://pandoc.org/filters.html) in
R.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rstudio/remarker")
```

## Overview

To understand what exactly remarker can do, it is important to
understand how R Markdown and regular Markdown documents get converted
to HTML or PDF.

When you call `rmarkdown::render()`, it does roughly this:

    R Markdown --[knitr]--> Markdown --[Pandoc]--> HTML

**knitr** reads in the R Markdown (Rmd) document, extracts R code chunks
and executes them, and the results are put back into a regular Markdown
file.

Then Pandoc reads in the Markdown file and converts it to HTML. To
expand on this a bit, it reads the Markdown, converts it internally to
the Pandoc AST (abstract syntax tree), and then the AST is converted to
HTML.

The Pandoc AST represents the structure of the document. For example, it
represents headers, paragraphs, and code blocks as objects in the tree.
The AST is the key data structure in Pandoc – it can convert many input
file formats to the AST, and it can convert the AST to many output
formats. These output formats include HTML, LaTeX, Microsft Word, and so
on.

Remarker provides a set of tools for manipulating the AST in R. This
allows one to modify and restructure documents programmatically before
they are converted to the output format. The transformations can be very
simple, like as converting italicized text to bold text, or they can be
more sophisticated, as in reordering sections, or inserting arbitrary
content.

Remarker fits into the toolchain like this:

    R Markdown
      --[R knitr]--> Markdown
      --[Pandoc]--> AST (JSON)
      --[R jsonlite::fromJSON()]--> AST (in R)
      --[R remarker user-defined transformations]--> AST (in R)
      --[R jsonlite::toJSON()]--> AST (JSON)
      --[Pandoc]--> HTML

## How it works

There are two common ways to use remarker. The most common way to use it
in a Pandoc filter. The other way is to use to manipulate an AST in an
interactive R session.

### Using remarker as a Pandoc filter

If you are writing an R Markdown .Rmd document (as opposed to a regular
Markdown .md document), the way to specify a filter is by adding the
following to the YAML header block:

``` yaml
output:
  html_document:
    pandoc_args: ["--filter=myfilter.R"]'
```

Note that the path to the filter is relative to the document’s path.
When `pandoc` is invoked to transform the Markdown to HTML, this means
it will be called with `--filter=myfilter.R`.

> Note: To use a filter that is part of an R package, you currently need
> to do something like this (hopefully we’ll find a better way to do
> it):

    ```YAML
        pandoc_args: !expr 'list(paste0("--filter=", system.file("filters/tabs.R", package="remarker")))'
    ```

The filter script should look something like this:

``` r
library(remarker)

script_filter(
  # Convert strings to lower case
  Str = function(x) {
    x$c <- tolower(x$c)
    x
  },
  # Convert italic text to bold
  Emph = function(x) {
    Strong(x$c)
  }
)
```

The API is similar to the [Pandoc Lua filter
API](https://pandoc.org/lua-filters.html).

### Using remarker interactively

For interactive use, here is one workflow:

-   If starting with an R Markdown file, use `knitr::knit()` to run the
    R code and emit a plain Markdown file.
-   Use `md_ast()`: this reads in the Markdown document and converts to
    the Pandoc AST. This requires Pandoc to be installed. (Under the
    hood, this runs Pandoc to read in the Md file and emit Pandoc AST in
    JSON format; then the R process ingests the JSON and converts it to
    an R data structure.)
-   Use `ast_filter()`: this applies *filter functions* on the Pandoc
    AST.
-   Use `ast_md()`, `ast_html()`: Convert the AST to Markdown or HTML.
    (This converts the R representation of the AST to JSON, then sends
    the JSON to Pandoc, which converts it to Markdown or HTML.)

## Playing with remarker

Remarker can be used to generate Pandoc AST for fragments of Markdown
content. To do this, call `md_ast()` and pass Markdown content as
`text`. For example:

``` r
library(remarker)
x <- md_ast(text =
"## Here's a section {.myclass prop=a}

Some *content* for the **section**.")

x
#> $`pandoc-api-version`
#> $`pandoc-api-version`[[1]]
#> [1] 1
#> 
#> $`pandoc-api-version`[[2]]
#> [1] 22
#> 
#> 
#> $meta
#> named list()
#> 
#> $blocks
#> 
#> [Blocks]
#> ├─1─{Block} t:Header  
#> │   └─c─[list]
#> │       ├─1─2
#> │       ├─2─[Attr]
#> │       │   ├─1─"heres-a-section"
#> │       │   ├─2─[Texts]
#> │       │   │   └─1─"myclass"
#> │       │   └─3─[TextText_s]
#> │       │       └─1─[TextText]
#> │       │           ├─1─"prop"
#> │       │           └─2─"a"
#> │       └─3─[Inlines]
#> │           └─1─{Inline} t:Str  c:"Here’s a section"
#> └─2─{Block} t:Para  
#>     └─c─[Inlines]
#>         ├─1─{Inline} t:Str  c:"Some "
#>         ├─2─{Inline} t:Emph  
#>         │   └─c─[Inlines]
#>         │       └─1─{Inline} t:Str  c:"content"
#>         ├─3─{Inline} t:Str  c:" for the "
#>         ├─4─{Inline} t:Strong  
#>         │   └─c─[Inlines]
#>         │       └─1─{Inline} t:Str  c:"section"
#>         └─5─{Inline} t:Str  c:"."
#> attr(,"class")
#> [1] "Pandoc"
```

Printing the returned Pandoc AST object will show the tree structure.
With the tree structure visible, you can easily extract and modify
components. For example, the tree shows the path to the italicized
string “content”:

``` r
x$blocks[[2]]$c[[2]]
#> 
#> {Inline} t:Emph  
#> └─c─[Inlines]
#>     └─1─{Inline} t:Str  c:"content"
```

This is an `Inline` object of type `Emph`, which in turn contains an
`Inlines`.
