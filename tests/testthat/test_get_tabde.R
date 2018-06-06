context("get_tabde")


test_that("get_tabde works as expected", {
  expect_identical(
    get_tabde(iris)$col_name,
    names(iris)
  )

  expect_identical(
    get_tabde(iris)$col_type,
    unname(vapply(iris, class, ""))
  )

  expect_true("table_design" %in% class(get_tabde(iris)))
})
