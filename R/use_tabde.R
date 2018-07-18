#' Create table design file
#'
#' `use_tabde()` is designed for use during package development.
#' If `x` is a `table_design()` it saves it directly to `inst/table_design/`.
#' If `x` is a normal `data.frame` it converts it to a `table_design`
#' via [get_tabde()] and then saves it.
#'
#' `use_tabde_fwf()` and `use_tabde_sql()` are convenince functions that do the
#' same, but in addition add the respective dummy columns.
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
