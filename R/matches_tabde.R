#' Title
#'
#' @param x
#' @param table_design
#'
#' @return
#' @export
#'
#' @examples
matches_tabde <- function(x, table_design){
  stopifnot(is_table_design(table_design))
  identical(names(x), table_design$col_name) &&
  identical(
    vapply(x, function(.) class(.)[[1]], "", USE.NAMES = FALSE),
    table_design$col_type
  )
}
