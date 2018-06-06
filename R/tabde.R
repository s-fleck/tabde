#' Table Design
#'
#' Table designs are `data.frames` that contain meta-info on other `data.frames`
#' such as column names and types. Those types can be used for consistency
#' checks (see [matches_tabde()]) and for generating sql `CREATE TABLE`
#' statements (see [as_sql()])
#'
#' @param col_names `character` vector. column names
#' @param col_types `character` vector. column types
#' @param ...
#'
#' @aliases table_design
#' @return a `data.frame` of class `table_design`
#' @export
#'
tabde <- function(
  col_name,
  col_type,
  ...
){
  stopifnot(do.call(is_equal_length, c(list(col_name, col_type), list(...))))

  res <- data.frame(
    col_name = col_name,
    col_type = col_type,
    ...,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  table_design(res)
}




#'
#' @rdname tabde
#' @param fwf_start `integer` vector. fwf start positons
#' @param fwf_end  `integer` vector. fwf end positons
#' @export
tabde_fwf <- function(
  col_name,
  col_type,
  fwf_start,
  fwf_end,
  ...
){
  res <- tabde(
    col_name  = col_name,
    col_type  = col_type,
    fwf_start = fwf_start,
    fwf_end   = fwf_end,
    ...
  )

  table_design_fwf(res)
}




#' @rdname tabde
#' @param sql_type  `character` vector. SQL Data Types as supported by target
#'   DBMS System.
#' @param sql_opts  `character` vector. SQL Options to be used by [as_sql()]
#'   (for example `NOT NULL`)
#' @export
tabde_sql <- function(
  col_name,
  col_type,
  sql_type,
  sql_opts = rep("", length(col_name)),
  ...
){
  res <- tabde(
    col_name = col_name,
    col_type = col_type,
    sql_type = sql_type,
    sql_opts = sql_opts,
    ...
  )

  table_design_sql(res)
}
