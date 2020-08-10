#' Convert table_design_fwf to col_positions for readr
#'
#' Convert [`table_design_fwf`] objects to lists that can be used as
#' `col_positions` argument to [readr::read_fwf()]
#'
#' @param x a `table_design_fwf` object.
#'
#' @return a `list` with columns `start`, `end`, `col_name`.
#' @export
#'
as_col_positions <- function(x){
  UseMethod("as_col_positions")
}




#' @rdname as_col_positions
#' @return a `list` with columns `start`, `end`, `col_name`
#' @export
as_col_positions.table_design_fwf <- function(x){
  list(
    begin = x$fwf_start,
    end   = x$fwf_end,
    col_names = x$col_name
  )
}
