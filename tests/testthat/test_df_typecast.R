context("df_typecast")







test_that("df_typecast_list: typecasting by name works.", {

  testdat <- data.frame(
    a = factor(c(6,5,3,4,5)),
    b = factor(c("one", "two", "three", "four", " apple ")),
    c = c(" one ", " two ", " three ", " four ", " apple "),
    d = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    e = c(1, 1, 1, 1, 1),
    f = c("moon", "moon", "moon", "moon", "moon"),
    g = c("TRUE", "TRUE", "TRUE", "FALSE", "FALSE"),
    h = c("3", "4", "5", "5", "5"),
    i = as.POSIXct(c("2015-01-01", "2015-01-05", "2015-05-04", "2015-12-01", "2015-04-13")),
    j = c("2015-01-01", "2015-01-05", "2015-05-04", "2015-12-01", "2015-04-13"),
    k = c("1", "1.5", "0.000000001", "100000000000", "99.1"),
    l = factor(c("1", "1.5", "0.000000001", "100000000000", "99.1")),
    m = factor(c("a", NA, NA, NA, NA)),
    n = factor(c(NA, NA, NA , NA, NA)),
    n2 = c("apple", "applepie", "moon", "nomoon", "moo"),
    stringsAsFactors = FALSE
  )

  # With list of col types
  res <- df_typecast_list(testdat, list(
    a = "numeric",
    b = "character",
    c = "factor",
    d = "factor",
    e = "integer",
    f = "factor",
    g = "logical",
    h = "integer",
    i = "character",
    j = c("POSIXct", "POSIXt")
  ))

  expect_identical(res$a, c(6, 5, 3, 4, 5))
  expect_identical(res$b, c("one", "two", "three", "four", " apple "))
  expect_identical(res$c, factor(testdat$c))
  expect_identical(res$d, factor(testdat$d))
  expect_identical(res$e, c(1L, 1L, 1L, 1L, 1L))
  expect_identical(res$f, factor(testdat$f))
  expect_identical(res$g, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(res$h, c(3L, 4L, 5L, 5L, 5L))
  expect_identical(
    res$i,
    c("2015-01-01", "2015-01-05", "2015-05-04", "2015-12-01", "2015-04-13")
  )


  # With table design
  td <- tabde::tabde(
    col_name = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
    col_type = c("numeric", "character", "factor", "factor", "integer", "factor", "logical", "integer", "character", "POSIXct")
  )

  res2 <- df_typecast(testdat, td)
  expect_identical(res, res2)
})




test_that("df_typecast_list: test conditions.", {

  tdat <- data.frame(
    a = c(
      NA, NA, "-1", "9999", "one", "6", NA, NA, "3", "1", "2", "1", "0", NA,
      NA, NA, NA, NA, NA, NA
    )
  )

  conv = list(a = "integer")
  expect_warning(
    res <- df_typecast_list(tdat, conv = conv),
    "NAs introduced by coercion"
  )

  expect_identical(
    res,
    data.frame(
      a = as.integer(c(
        NA, NA, -1L, 9999L, NA, 6L, NA,
        NA, 3L, 1L, 2L, 1L, 0L, NA, NA,
        NA, NA, NA, NA, NA))
    ))


  conv = list(a = "character", b = "numeric", c = "integer")
  expect_warning(
    res <- df_typecast_list(tdat, conv = conv),
    "Not all columns defined in conv are present in"
  )
})
