context("tabde")


test_that("tabde works as expected", {
  tres1 <- tabde(
    col_names = c("a", "b"),
    col_types = c("character", "numeric")
  )

  expect_true("table_design" %in% class(tres1))


  tres2 <- tabde_fwf(
    col_names = c("a", "b"),
    col_types = c("character", "numeric"),
    begin = c(1, 1),
    end = c(2, 2)
  )

  expect_true("table_design" %in% class(tres1))
  expect_true("table_design" %in% class(tres2))


  expect_error(
    tabde_fwf(
      col_names = c("a", "b"),
      col_types = c("character", "numeric"),
      begin = c(1, 1, 1),
      end = c(2, 2)
    )
  )
})
