assert_character <- function(
  x
){
  if (is.character(x))
    return(TRUE)

  msg <- paste0("`", deparse(substitute(x)), "` must be a character vector, not: ", preview_object(x))
  stop(TypeError(msg))
}


assert_equal_length <- function(
  ...
){
  if (is_equal_length(...))
    return(TRUE)

  nms <- paste0("`", as.character(as.list(match.call()[-1L])), "`")
  msg <- paste(comma(nms), "must be the same length.")
  stop(ValueError(msg))
}


TypeError <- function(message, ..., class = NULL, call = NULL){
  errorCondition(message = message, ..., class = union("TypeError", class), call = call)
}




ValueError <- function(message, ..., class = NULL, call = NULL){
  errorCondition(message = message, ..., class = union("ValueError", class), call = call)
}
