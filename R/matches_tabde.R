#' Check If a Data Frame Matches a Table Design
#'
#' If a columns has the `col_type` `NA` in the `table_design`, it will
#' be checked if the column is present in `x`, but the column type can be
#' arbitrary.
#'
#' If used with \pkg{assertthat}, `matches_tabde()` produces verbose error
#' messages.
#'
#' @param x a `data.frame`
#' @param table_design a [table_design]
#' @param skip `character` vector. Columns where the `col_type` in
#'   `table_design` matches any of the strings in `skip` will not be checked.
#'   see also [as_colspec()].
#'
#' @return `logical`
#' @export
#'
#' @examples
#' td <- get_tabde(iris)
#'
#' matches_tabde(iris, td)
#' matches_tabde(cars, td)
#'
#' \dontrun{
#'   library(assertthat)
#'   assert_that(matches_tabde(cars, td))
#' }
#'
matches_tabde <- function(
  x,
  table_design,
  skip = "#skip",
  values = NULL
){
  # precondtions
    stopifnot(is_table_design(table_design))

    if (!is.null(values)){
      if (!is_tabde_values(values)){
        stop("'values' must be a valid 'tabde_values' Object.")
      }

      if (has_domains(table_design)){
        "If 'values' is supplied, 'table_design' must have a 'col_domain' column"
      }
    }


  # logic
    cols_skip <- table_design$col_name[table_design$col_type %in% skip]
    x <- x[, !colnames(x) %in% cols_skip]
    table_design <- table_design[!table_design$col_type %in% skip, ]


    # col names
    if (!identical(names(x), table_design$col_name))
      return(FALSE)

    # col types
    col_types <- vapply(x, function(.) class(.)[[1]], "", USE.NAMES = FALSE)
    sel_types <- !is.na(table_design$col_type)
    if (!identical(col_types[sel_types], table_design$col_type[sel_types]))
      return(FALSE)

    # domains
    if (!is.null(values)){

      cols_to_check <- table_design[!is.na(table_design$col_domain), ]$col_name

      for (nm in cols_to_check) {
        dom  <- table_design[table_design$col_name == nm, "col_domain"]
        vals <- values[values$domain == dom, ]$value
        if (!all(x[[nm]] %in% vals)) return(FALSE)
      }
    }


  TRUE
}




attr(matches_tabde, "fail") <- function(call, env){

  xname <- deparse(call$x)
  x <- eval(call$x, envir = env)
  table_design <- eval(call$table_design, envir = env)
  values <- eval(call$values, envir = env)

  msg <- sprintf(
    "'%s' does not match table design:", xname
  )


  if (
    setequal(names(x), table_design$col_name) &&
    !identical(names(x), table_design$col_name)
  ) {
    msg["ord"] <- paste(
      "- column order does not match table design:",
      paste(" ", paste(colnames(x), collapse = ", ")),
      paste(" ", paste(table_design$col_name, collapse = ", ")),
      sep = "\n"
    )
  } else {
    missing_in_table_design <- setdiff(names(x), table_design$col_name)
    if (!is_empty(missing_in_table_design)){
      msg["mit"] <- paste(
        "- columns not in table design:",
        paste(missing_in_table_design, collapse = ", ")
      )
    }

    missing_in_x <- setdiff(table_design$col_name, names(x))
    if (!is_empty(missing_in_x)){
      msg["mix"] <- paste(
        sprintf("- columns not in '%s':", xname),
        paste(missing_in_x, collapse = ", ")
      )
    }
  }


  class_is     <- vapply(x, function(.) class(.)[[1]], "")
  class_should <- setNames(table_design$col_type, table_design$col_name)
  class_is     <- class_is[names(class_is) %in% names(class_should)]
  class_should <- class_should[names(class_should) %in% names(class_is)]

  sel <- class_is != class_should


  if (any(sel)){
    msg[["clas"]] <- paste(
      "- column types in 'x' do not match table design:",
      paste(
        sprintf(
          "%s (%s != %s)",
          names(class_is)[sel],
          class_is[sel],
          class_should[sel]
        ),
        collapse = ", "
      )
    )
  }


  if (!is.null(values) && has_domains(table_design)){
    for (nm in names(x)) {
      dom  <- table_design[table_design$col_name == "x", "col_domain"]
      vals <- values[values$domain == dom, ]$value
      if (!all(x[[nm]] %in% vals)) {
        msg[["dom"]] <- sprintf(
          "- column %s: values %s not in domain '%s'",
          nm,
          paste(setdiff(x[[nm]], vals), collapse = ", "),
          dom
        )
      }
    }
  }


  paste(msg, collapse = "\n")
}
