#' Read a table definition file
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
read_tabde <- function(
  file
){
  assert(is_scalar_character(file))

  if (grepl("\\.csvy$", file, ignore.case = TRUE))
    read_tabde_csvy(file)
  else
    read_tabde_csv(file)
}



read_tabde_csv <- function(
  file
){
  res <- utils::read.csv2(
    file,
    comment.char = "#",
    header = TRUE,
    row.names = NULL,
    stringsAsFactors = FALSE,
    na.strings = c("NA", ""),
    colClasses = c(
      col_name = "character",
      col_type = "character"
    )
  )

  res <- as_table_design(res)

  if ("fwf_start" %in% names(res)){
    res <- as_table_design_fwf(res)
  }

  if ("sql_type" %in% names(res)){
    res <- as_table_design_sql(res)
  }

  res
}



read_tabde_csvy <- function(file){
  assert_namespace("yaml")
  dd  <- readLines(file)
  sel <- grepl("^#", dd)
  header <- yaml::read_yaml(text = gsub("^#", "", dd[sel]))
  res    <- read_tabde_csv(file)

  if ("constraints" %in% names(header))
    attr(res, "constraints") <- as_tabde_constraints(header[["constraints"]])

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
  assert(is_table_design_sql(res))
  res
}




#' @rdname read_tabde
#' @export
#'
read_tabde_fwf <- function(file){
  res <- read_tabde(file)
  assert(is_table_design_fwf(res))
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
  assert(is_table_design(x))
  assert(is.data.frame(x))
  assert(is_scalar_logical(overwrite))

  if (!overwrite & file.exists(file)){
    stop("'", file, "' exists.")
  }

  utils::write.csv2(x, file = file, row.names = FALSE)
  invisible(file)
}
