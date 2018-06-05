# sfmisc utils 0.0.1.9000




# utils -------------------------------------------------------------------

compact <- function(x){
  x[!vapply(x, is.null, FALSE)]
}




walk <- function(.x, .f, ...){
  for (i in seq_along(.x)){
    .f(.x[[i]], ...)
  }

  invisible(.x)
}




# assertions --------------------------------------------------------------

assert_namespace <- function(x){
  stopifnot(requireNamespace(x, quietly = TRUE))
}




# predicates --------------------------------------------------------------

is_scalar_character <- function(x){
  is.character(x) && is_scalar(x)
}




is_scalar_logical <- function(x){
  is.logical(x) && is_scalar(x)
}




is_scalar_integerish <- function(x){
  is_scalar(x) && is_integerish(x)
}




is_scalar <- function(x){
  identical(length(x), 1L)
}




is_integerish <- function(x){
  if (!is.numeric(x)){
    vector("logical", length(x))
  } else {
    as.integer(x) == x
  }
}




is_equal_length <- function(...){
  lengths <- vapply(list(...), length, 1L)
  identical(length(unique(lengths)), 1L)
}




is_empty <- function(x){
  identical(length(x), 0L)
}




is_blank <- function(x){
  trimws(x) == ""
}
