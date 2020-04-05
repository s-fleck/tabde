#' @keywords internal
#' @rdname tabde
"_PACKAGE"




style_yellow <- function(...){
  if (requireNamespace("crayon", quietly = TRUE))
    crayon::yellow(...)
  else
    paste(...)
}


style_red <- function(x){
  if (requireNamespace("crayon", quietly = TRUE))
    crayon::red(...)
  else
    paste(...)
}
