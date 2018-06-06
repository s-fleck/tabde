#' Get table description from data.frame
#'
#' @param x a `data.frame`
#'
#' @return a `data.frame` with class [table_design]
#' @export
#'
#' @examples
#'
#' get_tabde(iris)
#'
get_tabde <- function(x){
  stopifnot(is.data.frame(x))

  tabde(
    col_name = names(x),
    col_type = vapply(x, function(.x) class(.x)[[1]], "")
  )
}
