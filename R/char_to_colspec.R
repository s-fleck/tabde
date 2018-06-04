#' Conver Verbal Data Type Description to `readr::col_spec`
#'
#' @param x a `character` vector of data types.
#'
#' @return a `col_spec` Objekt (see [readr::cols()])
#' @export
#'
#' @examples
#'
#' \dontrun{
#' char_to_colspec(c("integer", "logical", "numeric"))
#' }
#'
as_colspec <- function(x){
  assert_namespace("readr")
  UseMethod("as_colspec")
}




as_colspec.character <- function(x){
  assert_namespace("readr")

  y <- vector("list", length(x))
  y[x == "character"] <- list(readr::col_character())
  y[x == "numeric"]   <- list(readr::col_double())
  y[x == "double"]    <- list(readr::col_double())
  y[x == "integer"]   <- list(readr::col_integer())
  y[x == "logical"]   <- list(readr::col_logical())
  # y[x == "factor"]    <- list(readr::col_factor())
  y[x == "date"]      <- list(readr::col_date())
  y[x == "datetime"]  <- list(readr::col_datetime())
  y[x == "skip"]      <- list(readr::col_skip())
  y[x == "guess"]     <- list(readr::col_guess())
  y[x == "time"]      <- list(readr::col_time())


  assert_that(!any(vapply(y, is.null, FALSE)))

  do.call(readr::cols, y)
}
