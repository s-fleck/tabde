#' Read a table defintion file
#'
#' tabde recommends that you store table designs as semicolon (`;`) separated
#' csv files with header and without rownames. `read_tabde()` reads such files
#' and assigns the correct table_design subclasses based on the columns that
#' are present in `file` (f.e. `table_design_sql`, `table_design_fwf`).
#'
#'
#' @param file `character` scalar. File that contains a table design
#'
#' @return `read_tabde()` and friends return a `table_design data.frame`
#' @export
#'
read_tabde <- function(file){
  res <- utils::read.csv2(
    file,
    header = TRUE,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  res <- table_design(res)

  if ("fwf_start" %in% names(res)){
    res <- table_design_fwf(res)
  }

  if ("sql_type" %in% names(res)){
    res <- table_design_sql(res)
  }

  res
}




#' `read_table_sql()` and `read_table_fwf()` are more strict convience functions
#' that fail if `file` does not contain `sql` or `fwf`` specific columns.
#'
#' @rdname read_tabde
#' @export
#'
read_tabde_sql <- function(file){
  res <- read_tabde(file)
  stopifnot(is_table_design_sql(res))
  res
}




#' @rdname read_tabde
#' @export
#'
read_tabde_fwf <- function(file){
  res <- read_tabde(file)
  stopifnot(is_table_design_fwf(res))
  res
}




#' `write_tabde()` is a wrapper around `write.csv2()` that saves files in the
#' recommended format.
#'
#' @return `write_tabde()` and `use_tabde()` return `file` (invisibly)
#' @rdname read_tabde
#' @export
write_tabde <- function(
  x,
  file,
  overwrite = FALSE
){
  stopifnot(is.data.frame(x))
  stopifnot(is_scalar_logical(overwrite))

  if (!overwrite & file.exists(file)){
    stop("'", file, "' exists.")
  }

  utils::write.csv2(x, file = file, row.names = FALSE)
  invisible(file)
}




#' `use_tabde()` is designed for use during package development. It generates
#'   a table design from a `data.frame`, and saves it in `inst/table_design/`.
#'
#' @rdname read_tabde
#'
use_tabde <- function(
  x,
  file = NULL,
  overwrite = FALSE
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

  write_tabde(td, file = file, overwrite = overwrite)
  message("Saved table design to: ", file)


  invisible(file)
}


