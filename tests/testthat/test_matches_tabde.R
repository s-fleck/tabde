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

  td <- tabde::tabde(
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


  td <- tabde::tabde(
    col_name = c("Sepal.Width", "Sepal.Length", "Petal.Length", "Petal.Width", "Species"),
    col_type = c("numeric", "numeric", "numeric", "numeric", "factor")
  )

  expect_error(
    assertthat::assert_that(matches_tabde(iris, td)),
    "column order"
  )

})



test_that("matches_tabde handles NA and #skip gracefully", {

  td <- tabde::tabde(
    col_name = c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width", "Species"),
    col_type = c("numeric", "numeric", "#skip", NA, "factor")
  )

  expect_true(matches_tabde(iris, td))

  tdat <- iris
  tdat$Petal.Length <- NULL
  expect_true(matches_tabde(tdat, td))
 })




