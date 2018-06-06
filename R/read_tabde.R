#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
read_tabde <- function(file){
  res <- utils::read.csv2(
    file,
    header = TRUE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  res <- table_design(res)

  if ("fwf_start" %in% names(res)){
    res <- table_design_fwf(res)
  }

  if ("sql_type" %in% names(res)){
    res <- table_design_sql(res)
  }

  res
}




#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
read_tabde_sql <- function(file){
  res <- read_tabde(file)
  stopifnot(is_table_design_sql(res))
  res
}




#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
read_tabde_fwf <- function(file){
  res <- read_tabde(file)
  stopifnot(is_table_design_fwf(res))
  res
}
