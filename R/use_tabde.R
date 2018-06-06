#' Title
#'
#' @param x
#' @param file
#'
#' @return
#' @export
#'
#' @examples
use_tabde <- function(
  x,
  file = NULL
){
  assert_namespace("rprojroot")

  td <- tabde::get_tabde(x)


  if (is.null(file)){
    file <- rprojroot::find_package_root_file(
      "inst", "table_design", paste0(deparse(substitute(x)), ".csv")
    )
  }


  if (!dir.exists(dirname(file))){
    dir.create(dirname(file), recursive = TRUE)
  }

  write.csv2(td, file = file, row.names = FALSE)

  cat("Saved table design to:", file)


  invisible(file)
}



