#' Table design SQL Header
#'
#' @param const_name `character` vector. Name of the constraint.
#' @param const_type `character` vector. Type of the constraint. Currently
#'   the only supported value is `"PRIMARY KEY"` but foreign keys will be
#'   supported in the future
#' @param const_class `character` vector. Either `CONSTRAINT`, `PERIOD` or `RAW`.
#' @param const_cols a `list` of `character` vectors that must be the same
#'   length as `const_name`. If each constraint only consists of a single
#'   column, `const_cols` may also be a `character` vector of the same length
#'   as `const_name`.
#' @param ... extra columns to be added to the resulting data.frame
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' sql_header("PERSON_PK", "primary key", "name")
#' sql_header("PERSON_PK", "primary key", list(c("first_name", "last_name")))
sql_header <- function(
  const_name,
  const_type,
  const_cols,
  const_class = "constraint",
  ...
){
  if (is.character(const_cols))
    const_cols <- as.list(const_cols)

  assert(
    is_equal_length(const_name, const_type, const_cols),
    "`const_name`, `const_type` and `const_cols` must all be of the same length"
  )
  assert(is.character(const_name))
  assert(is.character(const_class))
  assert(is.character(const_type))
  assert(is.list(const_cols))

  structure(data.frame(
    const_name = const_name,
    const_class = const_class,
    const_type = const_type,
    const_cols = I(const_cols),
    stringsAsFactors = FALSE,
    row.names = NULL,
    ...
  ),
    class = c("sql_header", "data.frame")
  )
}



#' Coerce to sql_header
#'
#' @param x any supported \R object
#'
#' @return a `sql_header` object
#' @export
as_sql_header <- function(x){
  UseMethod("as_sql_header")
}





#' @rdname as_sql_header
#' @export
as_sql_header.list <- function(x){

  parse_raw <- function(.){
    list(
      columns = .,
      class = "raw",
      type = NA_character_
    )
  }

  parse_period <- function(.){
    list(
      class   = "period",
      type    = NA_character_,
      columns = .$columns
    )
  }

  parse_constraint <- function(.){
    list(
      class   = "constraint",
      type    = .$type,
      columns = .$columns
    )
  }

  res <- list()

  res[["raw1"]] <- lapply(
    x[setdiff(names(x), c("constraint", "period", "raw"))],
    parse_raw
  )

  if ("raw2" %in% names(x)){
    res[["raw"]] <- lapply(x[["raw"]], parse_raw)
  }

  if ("period" %in% names(x)){
    res[["period"]] <- lapply(x[["period"]], parse_period)
  }

  if ("constraint" %in% names(x)){
    res[["constraint"]] <- lapply(x[["constraint"]], parse_constraint)
  }

  res <- unlist(unname(res), recursive = FALSE)

  sql_header(
    const_name  = names(res),
    const_class = vapply(res, `[[`, character(1), "class", USE.NAMES = FALSE),
    const_type  = vapply(res, `[[`, character(1), "type", USE.NAMES = FALSE),
    const_cols  = unname(lapply(res, `[[`, "columns"))
  )
}




#' @rdname as_sql_header
#' @export
as_sql_header.data.frame <- function(x){
  misc_cols <- x[, !colnames(x) %in% c("const_name", "const_class", "const_type", "const_cols")]

  fct_to_char <- function(x) if (is.factor(x)) as.character(x) else (x)

  sql_header(
    const_name = fct_to_char(x$const_name),
    const_class = fct_to_char(x$const_class),
    const_type = fct_to_char(x$const_type),
    const_cols = fct_to_char(x$const_cols),
    misc_cols
  )
}




#' Convert sql_header to a character string
#'
#' @param x any \R object
#' @param ... ignored
#' @return a `character` scalar
#' @export
toString.sql_header <- function(
  x,
  ...
){
 paste(sql_create_table_sql_header(
   const_name = x$const_name,
   const_type = x$const_type,
   const_cols = x$const_cols,
   const_class = x$const_class
  ),
  collapse = "\n"
 )
}
