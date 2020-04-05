context("table_design")


test_that("table_design print method", {
  tres <- read_tabde(rprojroot::find_testthat_root_file("testdata", "iris.csvy"))
  expect_output(print(tres), "X_SPECIES_PK")
})
