#' Read csv files fast using the data.table package and table_designs
#'
#' A wrapper around [data.table::fread()] that automatically fills the
#' `col.names` and `colClasses` paramters from a table design.
#'
#' @inheritParams data.table::fread
#' @param table_design a [table_design]
#' @param ... passed on to [data.table::fread()]
#' @inherit data.table::fread return
#'
#' @export
#'
td_fread <- function(
  input = "",
  file,
  table_design,
  ...
){
  assert_namespace("data.table")

  data.table::fread(
    input,
    file,
    col.names = table_design$col_name,
    colClasses = table_design$col_type,
    ...
  )
}
