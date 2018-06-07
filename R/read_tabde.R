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
#' @param x a [table_design] Object
#' @param overwrite `scalar` character. Overwrite `file` if it exists?
#'
#' @return `write_tabde()` and `use_tabde()` return `file` (invisibly). This
#'   is useful for piping the saved file, for example into
#'   [\pkg{shed}](https://github.com/s-fleck/shed) (Shiny CSV Editor).
#'
#' @rdname read_tabde
#' @export
write_tabde <- function(
  x,
  file,
  overwrite = FALSE
){
  stopifnot(is_table_design(x))
  stopifnot(is.data.frame(x))
  stopifnot(is_scalar_logical(overwrite))

  if (!overwrite & file.exists(file)){
    stop("'", file, "' exists.")
  }

  utils::write.csv2(x, file = file, row.names = FALSE)
  invisible(file)
}




#' `use_tabde()` is designed for use during package development. If `x` is
#'   a `table_design()` it saves it directly to `inst/table_design/`. If `x`
#'   is a normal `data.frame` it converts it to a `table_design`
#'   via [get_tabde()] and then saves it
#'
#' @rdname read_tabde
#' @export
#'
use_tabde <- function(
  x,
  file = NULL,
  overwrite = FALSE
){
  assert_namespace("rprojroot")


  if (!is_table_design(x)){
    td <- tabde::get_tabde(x)
  } else {
    td <- x
  }


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


