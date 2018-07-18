#' Generate an SQL CREATE TABLE Statement From a tabde Table Design
#'
#' This does not perform any sanity check on the input data. `col_names` and
#' `sql_type` must be compatible with the DBMS that you want to use.
#'
#' @param x any \R object
#' @param ... passes on to methods
#'
#' @return `character` scalar. An SQL CREATE TABLE statement.
#' @export
#'
#' @examples
#'
#' td <- tabde_sql(
#'   col_name = c("numbers", "letters"),
#'   col_type = c("integer", "character"),
#'   sql_type = c("smallint", "varchar(255)")
#' )
#'
#' as_sql(td, "test_table")
#'
as_sql <- function(
  x,
  ...
){
  UseMethod("as_sql")
}




#' @param tname `character` scalar. name of target table
#' @rdname as_sql
#' @export
as_sql.table_design_sql <- function(
  x,
  tname,
  ...
){
  assert(is_scalar_character(tname))
  generate_sql_create_table(
    tname = tname,
    col_names = x$col_name,
    col_types = x$sql_type,
    sql_opts  = x$sql_opts
  )
}




#' Generate SQL CREATE TABLE statements
#'
#' Creates SQL CREATE TABLE statements from a vector of column names and
#' a vector of column types
#'
#' @param tname name of target sql table
#' @param col_names column names of target sql table
#' @param col_types column types of target sql table. Columns of type NA will
#'   be skipped
#' @param sql_opts column options of target sql table (for example `NOT
#'   NULL`)
#'
#' @return a `CREATE TABLE` statement as a text string
#' @export
#'
#' @examples
#'
#' generate_sql_create_table(
#'   "example.table",
#'   c("numbers", "animals"),
#'   c("integer", "varchar(8)"),
#'   c("NOT NULL", "")
#' )
#'
#'
generate_sql_create_table <- function(
  tname,
  col_names,
  col_types,
  sql_opts = rep("", length(col_names))
){
  # preconditions
  assert(is_scalar_character(tname))
  assert(is.character(col_names))
  assert(is.character(col_types))
  assert(is_equal_length(col_names, col_types, sql_opts))

  assert(all(
    is.na(col_names) == FALSE |
    is.na(col_names) == is.na(col_types)
  ))


  col_types  <- toupper(col_types)


  # process input
  empty_cols <- is.na(col_names) && is.na(col_types)
  col_names  <- col_names[!empty_cols]
  col_types  <- col_types[!empty_cols]

  if (any(is.na(col_types))){
    warning(sprintf(
      "Skipping %s columns with col_type 'NA'", sum(is.na(col_types))
    ))
    col_names <- col_names[!is.na(col_types)]
    col_types <- col_types[!is.na(col_types)]
    sql_opts  <- sql_opts[!is.na(col_types)]
  }


  cols <- paste0(
    trimws(paste0(col_names, " ", col_types, " ", sql_opts)),
    collapse = ", "
  )

  sprintf("CREATE TABLE %s (%s)", tname, cols)
}
