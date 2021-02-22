``` {.layout .grid}
|      |        |           |           |
|------|--------|-----------|-----------|
|1rem  |250px   |1fr        |1fr        |
|100px |header  |header     |header     |
|1fr   |sidebar |main_panel |main_panel |
|1fr   |sidebar |main_panel |main_panel |
```


## Header title here {#header}


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

This section is transformed into a **card**. The section heading is displayed with `H3` (instead of the normal `H2`), a horizontal rule is added below it, and the entire content is wrapped in a `wellPanel()`.

``` {.r .ui}
plotOutput("plot")
```

``` {.r .server}
output$plot <- renderPlot({
  plot(seq_len(input$x))
})
```
