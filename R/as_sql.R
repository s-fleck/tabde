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
  sql_create_table(
    tname = tname,
    col_names = x$col_name,
    col_types = x$sql_type,
    col_opts  = x$col_opts
  )
}




#' Generate SQL CREATE TABLE statements
#'
#' Creates SQL `CREATE TABLE` statements from a vector of column names and
#' a vector of column types
#'
#' @param tname `character` scalar. Name of target sql table
#' @param col_names `character` vector. Column names of target sql table
#' @param col_types `character` scalar. Column types of target sql table.
#'   Columns of type `NA` will be skipped
#' @param col_opts column options of target sql table (for example `NOT NULL`)
#'
#' @return a `CREATE TABLE` statement as a `character` scalar
#' @export
#'
#' @examples
#' sql_create_table(
#'   "example.table",
#'   c("numbers", "animals"),
#'   c("integer", "varchar(8)"),
#'   c("NOT NULL", "")
#' )
sql_create_table <- function(
  tname,
  col_names,
  col_types,
  col_opts = rep("", length(col_names)),
  const_names = NULL,
  const_types = NULL,
  const_cols = NULL
){
  # preconditions
  assert(
    is_scalar_character(tname)
  )

  cols <- sql_create_table_columns(col_names, col_types, col_opts)
  cols <- paste0(
    trimws(paste0(col_names, " ", col_types, " ", col_opts)),
    collapse = ", "
  )

  if (!is.null(const_names)){
    consts <- sql_create_table_constraints(
      const_names,
      const_types,
      const_cols
    )
  } else {
    assert(is.null(const_types), "If `const_names` is NULL, `const_types` must also be NULL, not", preview_object(const_types))
    assert(is.null(const_cols),  "If `const_cols` is NULL, `const_types` must also be NULL, not", preview_object(const_cols))
  }


  sprintf("CREATE TABLE %s (%s)", tname, cols)
}




sql_create_table_columns <- function(
  col_names,
  col_types,
  col_opts = rep("", length(col_names))
){
 # preconditions
  stopifnot(
    is.character(col_names),
    is.character(col_types),
    is_equal_length(col_names, col_types, col_opts)
  )

  assert(
    !anyNA(col_names) && all_are_distinct(col_names),
    "All `col_names` must be unique and non-`NA`"
  )

  col_opts[is.na(col_opts)] <- ""
  col_types  <- toupper(col_types)

  # process input
  if (any(is.na(col_types))){
    message(sprintf(
      "Skipping %s columns where `col_type` equals `NA`", sum(is.na(col_types))
    ))
    col_names <- col_names[!is.na(col_types)]
    col_types <- col_types[!is.na(col_types)]
    col_opts  <- col_opts[!is.na(col_types)]
  }

  trimws(paste0(col_names, " ", col_types, " ", col_opts))

}



sql_create_table_constraints <- function(
  const_names,
  const_types,
  const_cols
){
  stopifnot(
    is.character(const_names),
    is.character(const_types),
    is.list(const_cols),
    is_equal_length(const_names, const_types, const_cols)
  )

  assert(
    all(vapply(const_cols, is.character, logical(1))),
    "`cols` must be a list of `character` vectors"
  )

  assert(all_are_distinct(const_names))

  const_types <- toupper(const_types)

  assert(all(
    const_types == "PRIMARY KEY"
    # const_types == "FOREIGN KEY"
  ), "The only supported constraint types are 'PRIMARY KEY'")

  fmt_cols <- function(.) paste0("(", paste(., collapse = ", "), ")")

  paste("CONSTRAINT", const_names, const_types, vapply(const_cols, fmt_cols, character(1)))
}
