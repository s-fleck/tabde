#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
table_design <- function(x){
  class(x) <- union("table_design", class(x))
  x
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
table_design_fwf <- function(x){
  x <- table_design(x)
  class(x) <- union("table_design_fwf", class(x))
  x
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_df_tabde <- function(x){
  is.data.frame(x) &&
  ("col_name" %in% names(x)) &&
  ("col_type" %in% names(x))
}
