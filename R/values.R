#' Value-Domains
#'
#'
#' @param domain name oder id of the domain
#' @param value values that the `domains` can take
#'
#' @return ein `data.frame` der Klasse `domains`
#' @export
#'
values <- function(
  domain,
  value
){
  stopifnot(is.atomic(domain))
  stopifnot(is.atomic(value))

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
