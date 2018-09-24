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
  input,
  file,
  text,
  cmd,
  table_design,
  ...
){
  assert_namespace("data.table")

  assert(
    missing(input) + missing(file) + missing(text) + missing(cmd) == 3L,
    "Exaclty one of `input`, `file`, `text` or `cmd` must be spcifed."
  )

  if (!missing(input)){
    data.table::fread(
      input = input,
      col.names = table_design$col_name,
      colClasses = table_design$col_type,
      ...
    )

  } else if (!missing(file)){
    data.table::fread(
      file = file,
      col.names = table_design$col_name,
      colClasses = table_design$col_type,
      ...
    )

  } else if (!missing(text)){
    data.table::fread(
      file = text,
      col.names = table_design$col_name,
      colClasses = table_design$col_type,
      ...
    )

  } else if (!missing(cmd)){
    data.table::fread(
      cmd = cmd,
      col.names = table_design$col_name,
      colClasses = table_design$col_type,
      ...
    )
  }
}
