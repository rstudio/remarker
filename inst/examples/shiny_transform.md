``` {.layout .grid}
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
  sliderInput("x", "X:", 1, 10, 5),
  textInput("txt", "Input text")
)
```

------------------------------------------------------------------------

More text in the sidebar.

``` {.r .ui}
selectInput("select", "Select:", LETTERS)
```

## Main panel {#main_panel .card}

This section is transformed into a **card**. The section heading is displayed with `H3` (instead of the normal `H2`), and the entire content is wrapped in a `wellPanel()`.

``` {.r .ui}
plotOutput("plot")
```

``` {.r .server}
output$plot <- renderPlot({
  plot(seq_len(input$x))
})
```
