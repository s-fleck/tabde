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
#'   see also [as_col_spec()]
#' @param values a [values] Object. If `x` contains a `col_domain` column,
#'   valid values for each domain are looked up in `values`
#' @param check_nulls `logical`. If set to `TRUE` and `table_design` is a
#'   [table_design_sql] with an `"sql_opts"` column, this checks if `NOT NULL`
#'   columns contain NA
#'
#' @return `logical` scalar
#' @export
#'
#' @examples
#' td <- get_tabde(iris)
#'
#' matches_tabde(iris, td)
#' matches_tabde(cars, td)
#'
#' # Nice error messages with assertthat
#' if (requireNamespace("assertthat", quietly = TRUE)){
#'   try(assertthat::assert_that(matches_tabde(cars, td)))
#' }
#'
#'
#' td <- get_tabde_sql(iris)
#' td$sql_opts <- "NOT NULL"
#' x <- iris
#' x$Species[1] <- NA
#'
#' matches_tabde(x, td)
#' matches_tabde(x, td, check_nulls = TRUE)
#'
#' # Nice error messages with assertthat
#' if (requireNamespace("assertthat", quietly = TRUE)){
#'   try(assertthat::assert_that(matches_tabde(x, td, check_nulls = TRUE)))
#' }
#'
matches_tabde <- function(
  x,
  table_design,
  skip = "#skip",
  values = NULL,
  check_nulls = FALSE
){
  # precondtions
    stopifnot(
      is_table_design(table_design),
      is_scalar_character(skip),
      is_scalar_bool(check_nulls)
    )

    if (!is.null(values)){
      assert(
        is_tabde_values(values),
        "'values' must be a valid 'tabde_values' Object."
      )
      assert(
        has_domains(table_design),
        "If 'values' is supplied, 'table_design' must have a 'col_domain' column"
      )
    }

    if (check_nulls){
      assert(
        is_table_design_sql(table_design) & "sql_opts" %in% names(table_design),
        "When `check_nulls = TRUE`, 'table_design' must be a 'table_design_sql'",
        "that contains an sql_opts column."
      )
    }


  # logic
    cols_skip <- table_design$col_name[table_design$col_type %in% skip]
    x <- x[, !colnames(x) %in% cols_skip, drop = FALSE]
    table_design <- table_design[!table_design$col_type %in% skip, ]

    # col names
      if (!identical(names(x), table_design$col_name))
        return(FALSE)


    # col types
      col_type <- vapply(x, function(.) class(.)[[1]], "", USE.NAMES = FALSE)
      sel_types <- !is.na(table_design$col_type)
      if (!identical(col_type[sel_types], table_design$col_type[sel_types]))
        return(FALSE)


    # NAs
      if (check_nulls && !all(check_nulls(x, table_design)))
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




attr(matches_tabde, "fail") <- function(
  call,
  env
){
  # Deparse arguments to matches_tabde
    xname <- deparse(call$x)
    x <- eval(call$x, envir = env)
    table_design <- eval(call$table_design, envir = env)
    values <- eval(call$values, envir = env)
    check_nulls <- eval(call$check_nulls, envir = env)
    if (is.null(check_nulls))  check_nulls <- FALSE

    msg <- sprintf(
      "'%s' does not match table design:", xname
    )


  if (
    setequal(names(x), table_design$col_name) &&
    !identical(names(x), table_design$col_name)
  ){
    # Column order
    msg["ord"] <- paste(
      "- column order does not match table design:",
      paste(" ", paste(colnames(x), collapse = ", ")),
      paste(" ", paste(table_design$col_name, collapse = ", ")),
      sep = "\n"
    )
  } else {
    # Extra columns
      missing_in_table_design <- setdiff(names(x), table_design$col_name)
      if (!is_empty(missing_in_table_design)){
        msg["mit"] <- paste(
          "- columns not in table design:",
          paste(missing_in_table_design, collapse = ", ")
        )
      }

    # Missing columns
      missing_in_x <- setdiff(table_design$col_name, names(x))
      if (!is_empty(missing_in_x)){
        msg["mix"] <- paste(
          sprintf("- columns not in '%s':", xname),
          paste(missing_in_x, collapse = ", ")
        )
      }
  }


  # Column Types missmatch
    class_is     <- vapply(x, function(.) class(.)[[1]], "")
    class_should <- setNames(table_design$col_type, table_design$col_name)
    class_is     <- class_is[names(class_is) %in% names(class_should)]
    class_should <- class_should[names(class_should) %in% names(class_is)]
    sel <- class_is != class_should

    if (any(sel)){
      msg[["class"]] <- paste(
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


  # NAs/NULLs
    if (check_nulls){
      na_ok <- check_nulls(x, table_design)
      if (!all(na_ok)){
        msg[["nulls"]] <- paste(
        "- columns contain `NAs` but should be `NOT NULL`:",
        paste(names(na_ok)[!na_ok], collapse = ", ")
      )
      }
    }


  # Domains TODO:
    if (!is.null(values) && has_domains(table_design)){
      for (nm in names(x)) {
        dom  <- table_design[table_design$col_name == nm, "col_domain"]
        vals <- values[values$domain == dom, ]$value

        if (!is.na(dom) && !all(x[[nm]] %in% vals)) {
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



check_nulls <- function(
  x,
  table_design
){
  stopifnot(
    "sql_opts" %in% names(table_design),
    is.data.frame(x)
  )

  not_null_cols <-
    table_design$col_name[grep("NOT NULL", table_design$sql_opts)]

  vapply(
    names(x),
    function(nm) if (nm %in% not_null_cols) !anyNA(x[[nm]]) else TRUE,
    logical(1)
  )
}
