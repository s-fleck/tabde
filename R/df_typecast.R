#' Typecast columns of a data.frame
#'
#' Bulk-typecast columns of a `data.frame`. Use with care, will introduce NAs
#' for impossible casts (like the base \R as.* functions)
#'
#' `df_typecast()` uses a [table_design] Object to specify column types, while
#' `df_typecast_list()` uses a `list` (see examples).
#'
#'
#' @param x a `data.frame` or `list`
#' @param table_design a [table_design]
#' @param silent `logical`. Should warnings be displayed?
#'
#' @return Either a `data.frame` or `list` with typecasted columns/elements
#'   (depending on the type of `x`)
#' @export
#'
#' @examples
#'
#'
#' dat <- data.frame(
#'   foo = c('5', '6', '5'),
#'   bar = factor(c('a', 'b', 'c')),
#'   stringsAsFactors = FALSE
#' )
#' str(dat)
#'
#'
#' td <- tabde(
#'   col_name = c("foo", "bar"),
#'   col_type = c("character", "factor")
#' )
#' res <- df_typecast(dat, td)
#' str(res)
#'
#'
#' res <- df_typecast_list(
#'   dat,
#'   list(foo = "character", bar = "factor")
#' )
#' str(res)
#'
#'
#'
df_typecast <- function(
  x,
  table_design,
  silent = FALSE
){
  assert(is.list(x))
  assert(is_table_design(table_design))

  df_typecast_list(
    x,
    conv = structure(
      as.list(table_design$col_type),
      .Names = table_design$col_name
    ),
    silent = silent
  )
}






#' @param conv a list of the form list(colname = "coltype")
#' @rdname df_typecast
#' @export
#'
df_typecast_list <-  function(
  x,
  conv = list(),
  silent = FALSE
){
  conv2 <- conv[names(conv) %in% names(x)]

  if ( (length(conv2) < length(conv)) && !silent) {
    missing_cols <- names(conv)[!names(conv) %in% names(conv2)]
    warning(defined_column_is_missing_warning(missing_cols))
  }

  for (i in names(conv2)){
    toclass <- conv2[[i]]

    if ("POSIXct" %in% toclass){
      toclass <- "POSIXct"
    }

    f <- typecast_factory(toclass)

    if (any(class(x[[i]]) != toclass)) {

      x[[i]] <- tryCatch(
        f(x[[i]]),
        warning = function(w) {
          warning(typecast_produces_na_warning(
            i,
            class(x[[i]]),
            toclass,
            w$message
          ))
          suppressWarnings(f(x[[i]]))
        }
      )
    }
  }

  x
}




# utils -------------------------------------------------------------------

typecast_factory <- function(x){

  msg <- paste(
    "Input must be any of 'numeric', integer', 'factor'",
    "'character', 'POSIXct', 'integer64', 'Date', but is", x
  )

  res <- switch(
    x,
    "logical"   = as.logical,
    "integer"   = as.integer2,
    "integer64" = as.integer642,
    "factor"    = as.factor,
    "numeric"   = as.numeric2,
    "character" = as.character,
    "POSIXct"   = as.POSIXct,
    "Date"      = as.Date,
    stop(msg)
  )
  return(res)
}




as.numeric2   <- function(x) as.numeric(as.character(x))




as.integer2   <- function(x) as.integer(as.character(x))




as.integer642 <- function(x) {
  if (requireNamespace("bit64")){
    bit64::as.integer64(as.character(x))
  } else {
    stop("Requires the package bit64")
  }
}




# Conditions --------------------------------------------------------------
defined_column_is_missing_warning <- function(missing_cols) {
  mcs <- paste(missing_cols, collapse = ", ")
  msg <- sprintf(
    "Not all columns defined in conv are present in names(x): %s",
    mcs
  )

  condition(
    c("defined_column_is_missing_warning", "warning"),
    message = msg
  )
}




typecast_produces_na_warning <- function(col, fclass, tclass, text) {
  msg <- sprintf("%s(%s->%s): %s", col, fclass, tclass, text)
  condition(
    c("typecast_produces_na_warning", "warning"),
    message = msg
  )
}
