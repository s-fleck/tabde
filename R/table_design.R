#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'
as_table_design <- function(x){
  UseMethod("as_table_design")
}




#' @export
#'
as_table_design.data.frame <- function(x){
  assert(all(c("col_name", "col_type") %in% names(x)))
  assert(is.character(x$col_name))
  assert(is.character(x$col_type))

  if (!"table_design" %in% class(x)){
    class(x) <- union("table_design", class(x))
  }

  x
}




#' @export
#'
as_table_design_fwf <- function(x){
  UseMethod("as_table_design_fwf")
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_table_design_fwf.data.frame <- function(x){
  assert(all(c("fwf_start", "fwf_end") %in% names(x)))
  x <- as_table_design(x)
  class(x) <- union("table_design_fwf", class(x))
  x
}




#' @export
#'
as_table_design_sql <- function(x){
  UseMethod("as_table_design_sql")
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_table_design_sql.data.frame <- function(x){
  assert(all(c("sql_type", "sql_opts") %in% names(x)))
  x <- as_table_design(x)
  class(x) <- union("table_design_sql", class(x))
  x
}




#' @param x any \R Object
#' @rdname tabde
#' @export
is_table_design <- function(x){
  inherits(x, "table_design")
}




#' @rdname tabde
#' @export
#'
is_table_design_sql <- function(x){
  inherits(x, "table_design_sql")
}




#' @rdname tabde
#' @export
#'
is_table_design_fwf <- function(x){
  inherits(x, "table_design_fwf")
}




has_domains <- function(x){
  "col_domain" %in% colnames(x)
}
