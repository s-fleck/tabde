context("matches_tabde")


test_that("matches_tabde works as expected", {

  td <- tabde(
    col_name = c("blah", "blubb", "foo", "bar"),
    col_type = c("integer", "integer", "Date", "POSIXct")
  )

  dat <- data.frame(
    blah = as.numeric(1:2),
    blubb = 1:2,
    foo = Sys.Date(),
    bar = Sys.time()
  )
  expect_false(matches_tabde(dat, td))

  dat[[1]] <- as.integer(dat[[1]])
  expect_true(matches_tabde(dat, td))
})




test_that("matches_tabde produces informative error messages", {

  td <- tabde(
    col_name = c("Sepal.Length", "Sepal.Width", "blubb", "Species"),
    col_type = c("numeric", "integer", "blubb", "numeric")
  )

  expect_error(
    assertthat::assert_that(matches_tabde(iris, td)),
    "columns"
  )

  expect_error(
    assertthat::assert_that(matches_tabde(iris, td)),
    "column types"
  )

  expect_error(
    assertthat::assert_that(matches_tabde(iris, td)),
    "blubb"
  )


  td <- tabde(
    col_name = c("Sepal.Width", "Sepal.Length", "Petal.Length", "Petal.Width", "Species"),
    col_type = c("numeric", "numeric", "numeric", "numeric", "factor")
  )

  expect_error(
    assertthat::assert_that(matches_tabde(iris, td)),
    "column order"
  )

})



test_that("matches_tabde handles NA and #skip gracefully", {

  td <- tabde(
    col_name = c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width", "Species"),
    col_type = c("numeric", "numeric", "#skip", NA, "factor")
  )

  expect_true(matches_tabde(iris, td))

  tdat <- iris
  tdat$Petal.Length <- NULL
  expect_true(matches_tabde(tdat, td))
})




test_that("matches_tabde uses values", {

  tdat <- data.frame(
    x = c(LETTERS[c(4, 3, 2, 9, 10)]),
    y = c(LETTERS[c(23, 1, 3, 19, 21)]),
    stringsAsFactors = FALSE
  )

  td <- tabde(
    col_name = c("x", "y"),
    col_type = c("character", "character"),
    col_domain = c("letters", "letters")
  )

  ds <- values(
    "letters",
    LETTERS
  )

  expect_true(matches_tabde(tdat, td))
  expect_true(matches_tabde(tdat, td, values = ds))

  tdat$y <- c("bl", "b2", "b3", "b4", "b5")
  expect_false(matches_tabde(tdat, td, values = ds))

  expect_error(
    assertthat::assert_that(matches_tabde(tdat, td, values = ds)),
    "not in domain"
  )

  # NA domains are skipped
  expect_true(matches_tabde(tdat, td))
})




test_that("matches_tabde check NOT NULL sql_header", {

  td <- tabde_sql(
    col_name = c("blah", "blubb", "foo", "bar", "log"),
    col_type = c("numeric", "integer", "Date", "character", "logical"),
    sql_opts = c("NOT NULL", "", "", "NOT NULL", "NOT NULL")
  )

  dat <- data.frame(
    blah = as.numeric(1:2),
    blubb = 1:2,
    foo = Sys.Date(),
    bar = c("NA", "BLUBB", NA, NA),
    log = NA,
    stringsAsFactors = FALSE
  )

  expect_identical(
    unname(check_nulls(dat, td)),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  expect_true(
    matches_tabde(dat, td)
  )

  expect_false(
    matches_tabde(dat, td, check_nulls = TRUE)
  )

  expect_error(
    assertthat::assert_that(matches_tabde(dat, td, check_nulls = TRUE)),
    "bar, log"
  )
})

