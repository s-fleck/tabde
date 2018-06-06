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
  stopifnot(do.call(is_equal_length, c(list(col_names, col_types), list(...))))

  res <- data.frame(
    col_name = col_name,
    col_type = col_type,
    ...,
    stringsAsFactors = FALSE,
    row.names = NULL
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
  tabde(
    col_name = col_name,
    col_type = col_type,
    begin = begin,
    end = end,
    ...
  )
}
