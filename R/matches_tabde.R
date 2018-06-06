#' Check If a Data Frame Matches a Table Design
#'
#' If used with \pkg{assertthat}, `matches_tabde()` produces verbose error
#' messages.
#'
#' @param x a `data.frame`
#' @param table_design a [table_design]
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
matches_tabde <- function(x, table_design){
  stopifnot(is_table_design(table_design))
  identical(names(x), table_design$col_name) &&
  identical(
    vapply(x, function(.) class(.)[[1]], "", USE.NAMES = FALSE),
    table_design$col_type
  )
}





attr(matches_tabde, "fail") <- function(call, env){

  xname <- deparse(call$x)
  x <- eval(call$x, envir = env)
  table_design <- eval(call$table_design, envir = env)

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

  paste(msg, collapse = "\n")
}
