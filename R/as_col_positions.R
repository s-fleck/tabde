#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_col_positions <- function(x){
  UseMethod("as_col_positions")
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_col_positions.table_design_fwf <- function(x){
  list(
    begin = x$fwf_start,
    end   = x$fwf_end,
    col_names = x$col_name
  )
}
