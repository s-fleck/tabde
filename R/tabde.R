#' Title
#'
#' @param col_names
#' @param col_types
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tabde <- function(col_names, col_types, ...){

  res <- data.frame(
    col_names = col_names,
    col_types = col_types,
    ...
  )

  table_design(res)
}




#' Title
#'
#' @param col_names
#' @param col_types
#' @param begin
#' @param end
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tabde_fwf <- function(
  col_names,
  col_types,
  begin,
  end,
  ...
){
  res <- data.frame(
    col_names = col_names,
    col_types = col_types,
    ...
  )

  table_design(res)
}
