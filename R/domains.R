#' SDomains
#'
#'
#' @return eine `data.table` der Klasse `domains`
#' @export
#'
domains <- function(
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
    class = c("tabde_domains", "data.frame")
  )

  if(!identical(unique(res), res)){
    stop("All combinations of domain and value must be unique")
  }

  res

}




as_domains <- function(x){
  domains(x$domain, x$value)
}




is_tabde_domains <- function(x) inherits(x, "tabde_domains")
