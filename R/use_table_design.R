#' Title
#'
#' @param x
#' @param file
#'
#' @return
#' @export
#'
#' @examples
use_table_design <- function(x, file = deparse(substitute(x))){
  assert_namespace("rprojroot")

  td <- tabde::get_tabde(x)


  outfile <- rprojroot::find_package_root_file(
    "inst", "table_design", paste0(file, ".csv")
  )


  if (!dir.exists(dirname(outfile))){
    dir.create(dirname(outfile), recursive = TRUE)
  }

  write.csv2(td, file = outfile, row.names = FALSE)

  cat("Saved table design to:", outfile)


  invisible(outfile)
}
