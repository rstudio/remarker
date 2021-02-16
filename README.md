
<!-- README.md is generated from README.Rmd. Please edit that file -->

# remarker

<!-- badges: start -->
<!-- badges: end -->

**Remarker** is an R package for reading in Markdown files, applying
transformations, and emitting R code to

With remarker, you can write a Shiny application using Markdown, without
needing the complexity of rmarkdown’s `runtime: shiny` or
`shiny_prerendered`. The transformations will allow the application
author to modify and restructure objects in the document before they are
transformed to HTML.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("wch/remarker")
```

## Overview

Remarker operates in three phases:

-   `md_ast()`: Reads in the Markdown document and converting to the
    Pandoc AST. This requires Pandoc to be installed.
-   `ast_ir()`: Converts the Pandoc AST to remarker’s intermediate
    representation (IR).
-   `ir_transform()`: Applies user-defined transformations on the IR.
-   `ir_r()`: Converts the IR to R code which uses htmltools to do the
    HTML markup. This R code can then be executed.

## Examples

### A basic Shiny application

A Markdown file with a basic Shiny application is in
[inst/examples/shiny\_basic.md](inst/examples/shiny_basic.md). Here’s
how to read it in and convert it to the R code:

``` r
library(remarker)
md_file <- system.file("examples/shiny_basic.md", package = "remarker")

x <- md_ast(md_file) # Convert Markdown to Pandoc AST
x1 <- ast_ir(x)      # Convert to remarker IR
x2 <- ir_r(x1)       # Convert IR to R code
```

At this point, we can look at the resulting R code by simply printing
`x2`:

``` r
x2
#> ====================
#> UI code
#> ====================
#> fluidPage(
#>   sidebarPanel(
#>     tagList(
#>     h2(id = "sidebar", "Sidebar title here"),
#>     p("Here’s a sentence at the top of the sidebar."),
#>     {tagList(
#>   sliderInput("x", "X:", 1, 10, 5)
#> )},
#>     hr(),
#>     p(
#>       "Here’s more text in the sidebar. And a ",
#>       code("selectInput"),
#>       ":"
#>     ),
#>     {selectInput("select", "Select:", LETTERS)}
#>   )
#>   ),
#>   mainPanel(
#>     tagList(
#>     h2(id = "main_panel", "Hello"),
#>     p(
#>       "Here’s some text. Some of it is ",
#>       tags$i("italic"),
#>       "; some of it is ",
#>       tags$b("bold"),
#>       "; some of it is ",
#>       code("inlined code"),
#>       "."
#>     ),
#>     {plotOutput("plot")},
#>     p("This is a description of the plot above: it’s a bunch of points in a line."),
#>     NULL
#>   )
#>   )
#> )
#> 
#> ====================
#> Server code
#> ====================
#> function(input, output, session) {
#>   output$plot <- renderPlot({
#>     plot(seq_len(input$x))
#>   })
#> }
```

And this can be run as a Shiny application with `run_r()`:

``` r
run_r(x2)
```

It’s even possible to convert the IR

``` r
z <- md_ast("temp/simple_md.Rmd")
z1 <- ast_ir(z)
z2 <- ir_r(z1)
z2 %>% parse(text = .) %>% eval() %>% htmltools::browsable()
```
