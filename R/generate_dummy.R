#' Title
#'
#' @param table_design
#' @param n
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generate_dummy_df <- function(
  table_design,
  n = 6,
  ...
){
  l <- list()
  i_rows <- seq_len(nrow(table_design))
  sql_types <- tolower(table_design$sql_type)
  overrides <- list(...)

  if (length(overrides)){
    assert(all(names(overrides) %in% table_design$col_name))
    assert(all_are_distinct(names(overrides)))
  }

  for (i in i_rows){
    col <- table_design$col_name[[i]]
    st  <- sql_types[[i]]

    if (col %in% names(overrides)){
      l[[col]] <- overrides[[col]]

    } else if ("sql_type" %in% names(table_design)){
      l[[col]] <- generate_dummy_col(st, n = n)
    }
  }
  as.data.frame(l)
}




generate_dummy_col <- function(
  x = NULL,
  n = 6,
  type = NULL,
  size = NULL
){
  dummy_col_sql(sql_type = x, n = n, size = size, type = type)
}




parse_sql_type <- function(x){
  res <- strsplit(x, "(", fixed = TRUE)[[1]]

  if (identical(length(res), 1L)){
    size <- NULL
  } else {
    size <- as.integer(gsub(")", "", res[[2]], fixed = TRUE))
  }

  list(
    type = tolower(res[[1]]),
    size = size
  )
}




dummy_col_sql <- function(
  sql_type = NULL,
  n = 6,
  type = NULL,
  size = NULL
){
  assert(
    (!is.null(sql_type) && is.null(type) && is.null(size)) ||
    (is.null(sql_type) && !is.null(type) && !is.null(size)),
    "Either `sql_type` or `type`/`size` may be supplied, but not both."
  )

  if (!is.null(sql_type)){
    dd <- parse_sql_type(sql_type)
    type <- dd$type
    size <- dd$size
  }

  assert(is_scalar_integerish(n))
  assert(is_scalar_character(type))

  fun <- DUMMY_COL_GENERATORS[[type]] %||% na_col

  fun(n = n, size = size)
}




na_col <- function(n, ...){
  rep(NA, n)
}



random_int <- function(
  n = 1,
  size = NULL,
  min = NULL,
  max = NULL
){
  round(random_double(n = n, size = size, min = min, max = max))
}




random_binary <- function(n = 1, size = 1){
  sample(c(0, 1), size = n)
}




random_binary_char <- function(n = 1, size = 1){
  as.character(random_binary(n = n, size = size))
}




random_char <- function(n = 1, size = 8){
  replicate(n, paste0(sample(c(letters, LETTERS), size = size, replace = TRUE), collapse = ""))
}




random_double <- function(
  n,
  size = NULL,
  min = NULL,
  max = NULL
){
  if (is.null(size)){
    min <- 1
    max <- 1e8
  }

  runif(n = n, min, max)
}



DUMMY_COL_GENERATORS <- list(
  char = random_char,
  varchar = random_char,
  binary = random_binary,
  varbinary = random_binary,
  text = random_char,
  mediumtext = random_char,
  longtext = random_char,
  bit = random_int,
  tinytiny = random_int,
  bool = random_binary,
  boolean = random_binary,
  smallint = random_int,
  mediumint = random_int,
  int = random_int,
  bigint = random_int,
  float = random_double,
  double = random_double,
  `double precision` = random_double,
  decimal = random_int,
  dec = random_double
)
