#' Coerce object to table_design
#'
#' @param x any \R object
#' @param ... passed on to methods
#'
#' @return a `table_design` object of the appropriate subclass
#' @export
as_table_design <- function(
  x,
  ...
){
  UseMethod("as_table_design")
}



#' @rdname as_table_design
#' @export
as_table_design.data.frame <- function(
  x,
  ...
){
  assert(all(c("col_name", "col_type") %in% names(x)))
  assert(is.character(x$col_name))
  assert(is.character(x$col_type))

  if (!"table_design" %in% class(x)){
    class(x) <- union("table_design", class(x))
  }

  x
}



#' @rdname as_table_design
#' @export
as_table_design_fwf <- function(x){
  UseMethod("as_table_design_fwf")
}




#' @inheritParams as_table_design
#' @export
#'
as_table_design_fwf.data.frame <- function(x){
  assert(all(c("fwf_start", "fwf_end") %in% names(x)))
  x <- as_table_design(x)
  class(x) <- union("table_design_fwf", class(x))
  x
}




#' @param constraints a [tabde_constraints] object
#' @rdname as_table_design
#' @export
as_table_design_sql <- function(
  x,
  constraints = attr(x, "constraints")
){
  UseMethod("as_table_design_sql")
}




#' @inheritParams as_table_design
#' @export
as_table_design_sql.data.frame <- function(
  x,
  constraints = attr(x, "constraints")
){
  if (!is.null(constraints))
    constraints <- as_tabde_constraints(constraints)

  assert(all(c("sql_type", "sql_opts") %in% names(x)))
  x <- as_table_design(x)
  class(x) <- union("table_design_sql", class(x))
  attr(x, "constraints") <- constraints
  x
}




#' @rdname as_table_design
#' @export
is_table_design <- function(x){
  inherits(x, "table_design")
}




#' @rdname as_table_design
#' @export
is_table_design_sql <- function(x){
  inherits(x, "table_design_sql")
}




#' @rdname as_table_design
#' @export
#'
is_table_design_fwf <- function(x){
  inherits(x, "table_design_fwf")
}




has_domains <- function(x){
  "col_domain" %in% colnames(x)
}




#' Print a table_design object
#'
#' @param x any \R object
#' @param ... ignored
#' @export
#' @return `x` (invisibly)
print.table_design <- function(
  x,
  ...
){
  print.data.frame(x)
  const <- attr(x, "header")

  if (!is.null(const)){
    cat("\n")
    cat(style_yellow(toString(const)))
  }
  invisible(x)
}
