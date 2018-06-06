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
