#' Get table description from data.frame
#'
#' `get_tabde()` generates a table design from a `data.frame`. `get_tabde_sql()`
#' and `get_tabde_fwf()` do the same, but also add the respective dummy columns
#' for `table_design_sql` and `table_design_fwf` Objects.
#'
#' @param x a `data.frame`
#'
#' @return a `data.frame` with class [table_design]
#' @export
#'
#' @examples
#'
#' get_tabde(iris)
#' get_tabde_sql(iris)
#' get_tabde_fwf(iris)
get_tabde <- function(x){
  assert(is.data.frame(x))

  tabde(
    col_name = names(x),
    col_type = vapply(x, function(.x) class(.x)[[1]], "")
  )
}




#' @export
#' @rdname get_tabde
get_tabde_sql <- function(x){
  assert(is.data.frame(x))

  tabde_sql(
    col_name = names(x),
    col_type = vapply(x, function(.x) class(.x)[[1]], "")
  )
}




#' @export
#' @rdname get_tabde
get_tabde_fwf <- function(x){
  assert(is.data.frame(x))

  tabde_fwf(
    col_name = names(x),
    col_type = vapply(x, function(.x) class(.x)[[1]], ""),
    fwf_start = rep(NA_integer_, ncol(x)),
    fwf_end   = rep(NA_integer_, ncol(x))
  )
}
