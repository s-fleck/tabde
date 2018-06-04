#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
get_tabde <- function(x){

  data.frame(
    col_names = names(x),
    col_types = I(lapply(x, class)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
