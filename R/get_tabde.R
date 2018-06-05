#' Get table description from data.frame
#'
#' @param x a `data.frame`
#'
#' @return a `data.frame`  with the colums `col_names` and `col_types`
#' @export
#'
#' @examples
get_tabde <- function(x){
  stopifnot(is.data.frame(x))

  tabde(
    col_names = names(x),
    col_types = vapply(x, function(.x) class(.x)[[1]], "")
  )
}
