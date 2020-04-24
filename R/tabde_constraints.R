#' Table design SQL constraints
#'
#' @param const_name `character` vector. Name of the constraint.
#' @param const_type `character` vector. Type of the constraint. Currently
#'   the only supported value is `"PRIMARY KEY"` but foreign keys will be
#'   supported in the future
#' @param const_cols a `list` of `character` vectors that must be the same
#'   length as `const_name`. If each constraint only consists of a single
#'   column, `const_cols` may also be a `character` vector of the same length
#'   as `const_name`.
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' tabde_constraints("PERSON_PK", "primary key", "name")
#' tabde_constraints("PERSON_PK", "primary key", list(c("first_name", "last_name")))
tabde_constraints <- function(
  const_name,
  const_type,
  const_cols,
  ...
){
  assert(
    all(toupper(const_type) == "PRIMARY KEY"),
    "currently only 'primary key' constraints are supported"
  )

  if (is.character(const_cols))
    const_cols <- as.list(const_cols)

  assert(
    is_equal_length(const_name, const_type, const_cols),
    "`const_name`, `const_type` and `const_cols` must all be of the same length"
  )
  assert(is.character(const_name))
  assert(is.character(const_type))
  assert(is.list(const_cols))

  structure(data.frame(
    const_name = const_name,
    const_type = const_type,
    const_cols = I(const_cols),
    stringsAsFactors = FALSE,
    row.names = NULL,
    ...
  ),
    class = c("tabde_constraints", "data.frame")
  )
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_tabde_constraints <- function(x){
  UseMethod("as_tabde_constraints")
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_tabde_constraints.data.frame <- function(x){
  misc_cols <- x[, !colnames(x) %in% c("const_name", "const_type", "const_cols")]

  fct_to_char <- function(x) if (is.factor(x)) as.character(x) else (x)

  tabde_constraints(
    const_name = fct_to_char(x$const_name),
    const_type = fct_to_char(x$const_type),
    const_cols = fct_to_char(x$const_cols),
    misc_cols
  )
}




#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
as_tabde_constraints.list <- function(x){
  tabde_constraints(
    names(x),
    vapply(x, `[[`, character(1), "type", USE.NAMES = FALSE),
    unname(lapply(x, `[[`, "columns"))
  )
}



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
toString.tabde_constraints <- function(x){
  paste(
    pad_right(x$const_type),
    "\t",
    pad_right(x$const_name),
    "\t",
    vapply(x$const_cols, preview_object, character(1))
  )
}
