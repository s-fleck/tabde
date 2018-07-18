#' Table Design
#'
#' Table designs are `data.frames` that contain meta-info on other `data.frames`
#' such as column names and types. Those types can be used for consistency
#' checks (see [matches_tabde()]) and for generating sql `CREATE TABLE`
#' statements (see [as_sql()])
#'
#' @param col_name `character` vector. column names
#'
#' @param col_type `character` vector. Valid `data.frame` column types. `NA`s
#'   and the string `"#skip"` have special meanings.
#'
#'   `NA`s will be converted to `readr::col_guess()` by [as_colspec()] and
#'   [matches_tabde()] will not check the classes of `NA` columns. In a similar
#'   fashion `#skip` columns will be convertet to `readr::col_skip()` and
#'   `matches_tabde` will not check if they are present in `dat` or not.
#'
#'
#' @param col_domain `character` vector. A valid domain for `col_name`. Domains
#'   can be used by [`matches_tabde()`] and [`as_sql()`] to check for valid
#'   (discrete) values if a [`values`] Object is passed to these functions.
#'
#' @param ... passed on to methods
#'
#' @aliases table_design table_design_fwf table_design_sql
#' @return a `data.frame` of class `table_design`
#' @export
#' @rdname tabde
#'
tabde <- function(
  col_name,
  col_type = rep(NA_character_, length(col_name)),
  col_domain = NULL,
  ...
){
  if (is.null(col_domain)){
    assert(do.call(
      is_equal_length,
      c(list(col_name, col_type), list(...))
    ))

    res <- data.frame(
      col_name = col_name,
      col_type = col_type,
      ...,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  } else {
    assert(do.call(
      is_equal_length,
      c(list(col_name, col_type, col_domain), list(...))
    ))

    res <- data.frame(
      col_name = col_name,
      col_type = col_type,
      col_domain = col_domain,
      ...,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }


  table_design(res)
}





#' @rdname tabde
#' @param fwf_start `integer` vector. fwf start positons
#' @param fwf_end  `integer` vector. fwf end positons
#' @export
tabde_fwf <- function(
  col_name,
  col_type = rep(NA_character_, length(col_name)),
  fwf_start,
  fwf_end,
  col_domain = NULL,
  ...
){
  res <- tabde(
    col_name  = col_name,
    col_type  = col_type,
    fwf_start = fwf_start,
    fwf_end   = fwf_end,
    col_domain = col_domain,
    ...
  )

  table_design_fwf(res)
}




#' @rdname tabde
#' @param sql_type  `character` vector. SQL Data Types as supported by target
#'   DBMS System. Columns wil col_type `NA` will be skipped when creating
#'   SQL statements.
#' @param sql_opts  `character` vector. SQL Options to be used by [as_sql()]
#'   (for example `NOT NULL`)
#' @export
tabde_sql <- function(
  col_name,
  col_type = rep(NA_character_, length(col_name)),
  sql_type = rep(NA_character_, length(col_name)),
  sql_opts = rep(NA_character_, length(col_name)),
  col_domain = NULL,
  ...
){
  res <- tabde(
    col_name = col_name,
    col_type = col_type,
    sql_type = sql_type,
    sql_opts = sql_opts,
    col_domain = col_domain,
    ...
  )

  table_design_sql(res)
}
