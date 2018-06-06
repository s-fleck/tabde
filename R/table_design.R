#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
table_design <- function(x){
  stopifnot(all(c("col_name", "col_type") %in% names(x)))
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
  stopifnot(all(c("fwf_start", "fwf_end") %in% names(x)))
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
table_design_sql <- function(x){
  stopifnot(all(c("sql_type", "sql_opts") %in% names(x)))
  x <- table_design(x)
  class(x) <- union("table_design_sql", class(x))
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
is_table_design_sql <- function(x){
  inherits(x, "table_design_sql")
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_table_design_fwf <- function(x){
  inherits(x, "table_design_fwf")
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_table_design <- function(x){
  inherits(x, "table_design")
}
