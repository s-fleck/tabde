#' Value-Domains For Columns in Table Designs
#'
#'
#' @param domain name oder id of the domain.
#' @param value values that the `domains` can take
#'
#' @return a `data.frame` of class `"tabde_values"`
#' @export
#'
#' @examples
#'
#' vals <- values(
#'   c("letters", "letters", "numbers"),
#'   c("a", "b", 2)
#' )
#'
#' td <- tabde(
#'   col_name   = c("Alpha", "Num"),
#'   col_type   = c("character", "numeric"),
#'   col_domain = c("letters", "numbers")
#' )
#'
#' df <- data.frame(
#'   Alpha = c("a", "b", "c"),
#'   Num   = c(2, 2, 3),
#'   stringsAsFactors = FALSE
#' )
#'
#'
#' # Fails because df contains values not in `vals`:
#' matches_tabde(df, td, values = vals)
#'
#' # Ok:
#' matches_tabde(df[1:2, ], td, values = vals)
#'
#' # domains are only checked if `values` is passed to matches_tabde
#' matches_tabde(df, td)
values <- function(
  domain,
  value
){
  assert(is.atomic(domain))
  assert(is.atomic(value))

  res <- structure(
    data.frame(
      domain = domain,
      value = value,
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    class = c("tabde_values", "data.frame")
  )

  if(!identical(unique(res), res)){
    stop("All combinations of domain and value must be unique")
  }

  res
}




as_domains <- function(x){
  domains(x$domain, x$value)
}




is_tabde_values <- function(x) inherits(x, "tabde_values")
