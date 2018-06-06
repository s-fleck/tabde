context("read_tabde")


test_that("read_tabde works as expected", {
  tres <- read_tabde(rprojroot::find_testthat_root_file("testdata", "iris.csv"))

  expect_identical(
    class(tres),
    c("table_design_sql", "table_design_fwf", "table_design", "data.frame")
  )


})
