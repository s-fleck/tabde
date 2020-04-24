#' Use table design file
#'
#'
#' @description
#'
#' `use_tabde()` is designed to be used during package development. It saves
#' `table_designs` to the `inst/table_design/` folder of the current
#' package (and creates this folder if it doesn't exist).
#'
#' `use_tabde_fwf()` and `use_tabde_sql()` are convenience functions that do the
#' same and add the respective dummy columns.
#'
#' @param x a [`table_design`] or a regular `data.frame`. Regular `data.frames`
#'   are  converted to `table_designs` via [get_tabde()].
#' @param file scalar `character`. name for the table_design `.csv` file. The
#'   default is to construct a filename from the name of the object passed to
#'   `x`.
#' @param overwrite `logical` scalar. Set `TRUE` to overwrite existing files.
#'
#' @rdname use_tabde
#' @export
#'
use_tabde <- function(
  x,
  file = {
    if (assert_namespace("rprojroot"))
      rprojroot::find_package_root_file("inst", "table_design", paste0(deparse(substitute(x)), ".csv"))
    else
      stop("argument 'file' is missing")
  },
  overwrite = FALSE
){
  use_tabde_internal(
    x = x,
    file = file,
    overwrite = overwrite,
    fun = get_tabde
  )
}




#' @rdname use_tabde
#' @export
use_tabde_fwf<- function(
  x,
  file = {
    if (assert_namespace("rprojroot"))
      rprojroot::find_package_root_file("inst", "table_design", paste0(deparse(substitute(x)), ".csv"))
    else
      stop("argument 'file' is missing")
  },
  overwrite = FALSE
){
  use_tabde_internal(
    x = x,
    file = file,
    overwrite = overwrite,
    fun = get_tabde_sql
  )
}




#' @rdname use_tabde
#' @export
use_tabde_sql <- function(
  x,
  file = {
    if (assert_namespace("rprojroot"))
      rprojroot::find_package_root_file("inst", "table_design", paste0(deparse(substitute(x)), ".csv"))
    else
      stop("argument 'file' is missing")
  },
  overwrite = FALSE
){
  use_tabde_internal(
    x = x,
    file = file,
    overwrite = overwrite,
    fun = get_tabde_sql
  )
}




use_tabde_internal <- function(
  x,
  file,
  overwrite,
  fun
){
  assert_namespace("rprojroot")
  assert(is_scalar_character(file))
  assert(is_scalar_logical(overwrite))
  assert(is.function(fun))


  if (!is_table_design(x)){
    td <- fun(x)
  } else {
    td <- x
  }


  if (!dir.exists(dirname(file))){
    dir.create(dirname(file), recursive = TRUE)
  }

  write_tabde(td, file = file, overwrite = overwrite)
  message("Saved table design to: ", file)


  invisible(file)
}
