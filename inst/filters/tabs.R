#!/usr/bin/env Rscript
#
# Usage:
#   pandoc -f markdown -t markdown --filter tabs.R -s hello.md

library(remarker)


tabset_filter <- function(x) {
  if (! "tabs" %in% el_get_class(x$c[[1]])) {
    return()
  }

  tabset_fn <- shiny::tabsetPanel
  key_vals <- el_get_keyvals(x$c[[1]])
  if ("tabtype" %in% names(key_vals)) {
    switch(key_vals[["tabtype"]],
      tabsetPanel = {
        tabset_fn <- shiny::tabsetPanel
      },
      navlistPanel = {
        tabset_fn <- shiny::navlistPanel
      },
      {
        stop("tabtype is unkown")
      }
    )
  }

  tabset_id <- x$c[[1]]$c[[2]][[1]]

  content <- x$c[[2]]

  tabitems <- lapply(content, function(tabitem) {
    if (tabitem$t == "NestedSection") {

      if ("tabset2" %in% el_get_class(tabitem$c[[1]])) {
        # Handle nested .tabset2
        return(tabset_filter(tabitem))
      }

      # Extract information for the call to tabPanel()
      id <- tabitem$c[[1]]$c[[2]][[1]]
      title <- tabitem$c[[1]]$c[[3]]
      content_html <- tabitem$c[[2]]

      shiny::tabPanel(title = title, content_html, value = id)

    } else {
      stop("All information must be in tabs")
    }
  })

  res <- do.call(tabset_fn, c(tabitems, id = tabset_id))

  res <- mixed_to_rawblocks(res)
  res
}


script_filter(
  list(Pandoc = nest_sections),
  list(NestedSection = tabset_filter),
  list(Pandoc = unnest_sections)
)
