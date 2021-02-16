
``` {.r .layout}
fluidPage(
  sidebarPanel(
    {{ sidebar }}
  ),
  mainPanel(
    {{ main_panel }}
  )
)
```

## Sidebar title here {#sidebar}

Here's a sentence at the top of the sidebar.

``` {.r .ui}
tagList(
  sliderInput("x", "X:", 1, 10, 5)
)
```

*****

Here's more text in the sidebar. And a `selectInput`:

``` {.r .ui}
selectInput("select", "Select:", LETTERS)
```


## Hello {#main_panel}

Here's some text. Some of it is _italic_; some of it is **bold**; some of it is `inlined code`.

``` {.r .ui}
plotOutput("plot")
```

This is a description of the plot above: it's a bunch of points in a line.

``` {.r .server}
output$plot <- renderPlot({
  plot(seq_len(input$x))
})
```
