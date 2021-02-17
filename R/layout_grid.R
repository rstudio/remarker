#' Build grid layout obj from markdown table
#'
#' @param layout_table Character string with a markdown table. First row and
#'   column are reserved for sizing (any valid css sizing works). An optional
#'   grid-gap can be provided using the very first cell.
#'
#' @return An object of class "grid_layout", which stores the layout as a
#'   matrix. This can be passed to other functions such as `layout_to_css()`.
#' @export
#'
#' @examples
#' grid_obj <- grid_layout_from_md(
#'   layout_table = "
#'     |      |120px   |1fr    |1fr    |
#'     |:-----|:-------|:------|:------|
#'     |100px |header  |header |header |
#'     |1fr   |sidebar |plot_a |plot_c |
#'     |1fr   |sidebar |plot_b |plot_b |"
#' )
#'
grid_layout_from_md <- function(layout_table){

  by_row <- strsplit(layout_table, "\n")[[1]]
  is_header_divider <- grepl("^[\\| \\- :]+$", by_row, perl = TRUE)
  is_empty_row <- by_row == ""
  clean_rows <- by_row[!(is_header_divider | is_empty_row)]

  # Get rid of empty first el caused by table boundaries
  raw_mat <- t(sapply(
    strsplit(clean_rows, "\\s*\\|\\s*", perl = TRUE),
    function(row){row[-1]}
  ))

  grid_mat <- raw_mat[-1,-1]
  colnames(grid_mat) <- raw_mat[1,-1]
  rownames(grid_mat) <- raw_mat[-1,1]
  grid_gap <- if(raw_mat[1,1] != "") raw_mat[1,1] else "1rem"

    class(grid_mat) <- "grid_layout"
  attr(grid_mat, "gap") <- grid_gap
  attr(grid_mat, "source") <- layout_table
  grid_mat
}


#' Convert grid layout to css string
#'
#' @param layout Object of class "grid_layout".
#' @param container_query Query used by css to find the container for your grid.
#'   Defaults to `"body"` for whole page grids
#'
#' @return Character string of css used to setup grid layout and place elements
#'   (referenced by id) into correct locations
#' @export
#'
#' @examples
#'
#' grid_obj <- grid_layout_from_md(
#'   layout_table = "
#'     |      |120px   |1fr    |1fr    |
#'     |:-----|:-------|:------|:------|
#'     |100px |header  |header |header |
#'     |1fr   |sidebar |plot_a |plot_c |
#'     |1fr   |sidebar |plot_b |plot_b |"
#' )
#'
#' layout_to_css(grid_obj)
#'
layout_to_css <- function(layout, container_query = "body"){

  collapse_w_space <- function(vec) { paste(vec, collapse = " ") }

  template_areas <- ""
  for(row_i in 1:nrow(layout)){
    current_row <- collapse_w_space(layout[row_i,])
    template_areas <- paste0(template_areas, "\n    \"", current_row, "\"")
  }

  # Build the mapping for each element to its grid area
  element_grid_areas <- paste(
    sapply(
      unique(c(layout)),
      function(id){
        glue::glue("#{id} {{ grid-area: {id}; }}")
      }
    ),
    collapse = "\n"
  )

  glue::glue("
{container_query} {{
  grid-template-rows: {collapse_w_space(rownames(layout))};
  grid-template-columns: {collapse_w_space(colnames(layout))};
  grid-template-gap: {attr(layout, 'gap')};
  padding: {attr(layout, 'gap')};
  grid-template-areas: {template_areas};
}}

{element_grid_areas}
")
}
