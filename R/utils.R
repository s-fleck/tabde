assert_namespace <- function(x){
  stopifnot(requireNamespace(x, quietly = TRUE))
}
