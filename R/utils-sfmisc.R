# sfmisc utils 0.0.1.9002




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
  invisible(TRUE)
}





# conditions --------------------------------------------------------------

#' Condition constructor
#'
#' A constructur function for conditions, taken from
#' \url{http://adv-r.had.co.nz/beyond-exception-handling.html}
#'
#' @param subclass Subclass to assign to the condition
#' @param message  message to be passed to the condition
#' @param call     call passed on to the conditon
#' @param ...      further list elements to be passed on to the resulting object
#'
#' @return a condition object
#' @noRd
#'
#' @examples
#'
#' \dontrun{
#' # Construct a custom condition
#' malformed_log_entry_error <- function(text) {
#'   msg <- paste0("Malformed log entry: ", text)
#'   condition(
#'     c("malformed_log_entry_entry", "error"),
#'     message = msg,
#'     text = text
#'   )
#' }
#'
#'
#' # Signal the condition
#' parse_log_entry <- function(text) {
#'   if (!well_formed_log_entry(text)) {
#'     stop(malformed_log_entry_error(text))
#'    }
#' }
#'
#'
#' # Handle the condition
#' tryCatch(
#'   malformed_log_entry = function(e) NULL,
#'   parse_log_entry(text)
#' )
#' }
#'
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}




#' @export
#' @rdname condition
error <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "error", 'condition'),
    list(message = message, call = call, ...)
  )
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
