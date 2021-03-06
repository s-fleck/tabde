#' Generate an SQL CREATE TABLE Statement From a tabde Table Design
#'
#' This does not perform any sanity check on the input data. `col_name` and
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
  sql_header <- attr(x, "sql_header")

  if (!is.null(sql_header)){
    sql_create_table(
      tname = tname,
      col_name   = x$col_name,
      col_type   = x$sql_type,
      col_opts   = x$sql_opts,
      const_name = sql_header$const_name,
      const_class = sql_header$const_class,
      const_type = sql_header$const_type,
      const_cols = sql_header$const_cols
    )
  } else {
    sql_create_table(
      tname = tname,
      col_name = x$col_name,
      col_type = x$sql_type,
      col_opts  = x$sql_opts
    )
  }
}




#' Generate SQL CREATE TABLE statements
#'
#' Creates SQL `CREATE TABLE` statements from a vector of column names and
#' a vector of column types
#'
#' @param tname `character` scalar. Name of target sql table
#' @param col_name `character` vector. Column names of target sql table
#' @param col_type `character` scalar. Column types of target sql table.
#'   Columns of type `NA` will be skipped
#' @param col_opts column options of target sql table (for example `NOT NULL`)
#' @inheritParams sql_header
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
  col_name,
  col_type,
  col_opts = rep("", length(col_name)),
  const_name  = NULL,
  const_type  = NULL,
  const_cols  = NULL,
  const_class = "constraint"
){
  # preconditions
  assert(is_scalar_character(tname))

  els <- sql_create_table_columns(col_name, col_type, col_opts)

  if (!is.null(const_name)){
    consts <- sql_create_table_sql_header(
      const_name = const_name,
      const_type = const_type,
      const_cols = const_cols,
      const_class = const_class
    )

    els <- c(els, consts)

  } else {
    assert(is.null(const_type), "If `const_name` is NULL, `const_type` must also be NULL, not", preview_object(const_type))
    assert(is.null(const_cols),  "If `const_cols` is NULL, `const_type` must also be NULL, not", preview_object(const_cols))
  }

  els <- paste(els,  collapse = ", ")

  sprintf("CREATE TABLE %s (%s)", tname, els)
}




sql_create_table_columns <- function(
  col_name,
  col_type,
  col_opts = rep("", length(col_name))
){
 # preconditions
  assert_character(col_name)
  assert_character(col_type)

  if (all(is.na(col_opts))){
    col_opts <- rep("", length(col_opts))
  }

  assert_character(col_opts)
  assert_equal_length(col_name, col_type, col_opts)

  assert(
    !anyNA(col_name) && all_are_distinct(col_name),
    "All `col_name` must be unique and non-`NA`"
  )

  col_opts[is.na(col_opts)] <- ""
  col_type  <- toupper(col_type)

  # process input
  if (any(is.na(col_type))){
    message(sprintf(
      "Skipping %s columns where `col_type` equals `NA`", sum(is.na(col_type))
    ))
    col_name <- col_name[!is.na(col_type)]
    col_type <- col_type[!is.na(col_type)]
    col_opts  <- col_opts[!is.na(col_type)]
  }

  trimws(paste0(col_name, " ", col_type, " ", col_opts))
}



sql_create_table_sql_header <- function(
  const_name,
  const_type,
  const_cols,
  const_class = "constraint"
){
  stopifnot(
    is.character(const_name),
    is.character(const_class),
    is.character(const_type),
    is.list(const_cols),
    is_equal_length(const_name, const_type, const_cols)
  )

  assert(
    all(vapply(const_cols, function(.) is.null(.) || is.character(.) , logical(1))),
    "`const_cols` must be a list of `character` vectors"
  )

  assert(all_are_distinct(const_name))
  const_type  <- tolower(const_type)
  const_class <- tolower(const_class)

  fmt_cols <- function(.){
    if (is.null(.))
      NULL
    else
      paste0("(", paste(., collapse = ", "), ")")
  }

  if (!length(const_name))
    return(list())

  mapply(
    function(name, class, type, cols){
      if (class == "raw"){
        trimws(paste(name, fmt_cols(cols)))
      } else {
        if (is.na(type)){
          trimws(paste(class, name, fmt_cols(cols)))
        } else {
          trimws(paste(class, name, type, fmt_cols(cols)))
        }

      }
    },
    const_name, const_class, const_type, const_cols,
    USE.NAMES = FALSE
  )
}
