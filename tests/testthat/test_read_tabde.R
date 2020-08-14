context("read_tabde")


test_that("read_tabde works as expected", {
  tres <- read_tabde(rprojroot::find_testthat_root_file("testdata", "iris.csv"))

  expect_identical(
    class(tres),
    c("table_design_sql", "table_design_fwf", "table_design", "data.frame")
  )
})




test_that("read_tabde works for csvy files", {
  tres <- read_tabde(rprojroot::find_testthat_root_file("testdata", "iris.csvy"))

  expect_identical(
    as_sql(tres, "test.table"),
    "CREATE TABLE test.table (Sepal.Length DOUBLE NOT NULL, Sepal.Width DOUBLE, Petal.Length DOUBLE, Petal.Width DOUBLE, Species VARCHAR(255), CONSTRAINT X_SPECIES_PK PRIMARY KEY (Species))"
  )
})




test_that("sql_create_table_columns fails gracefully", {
  x <- read_tabde_csvy(rprojroot::find_testthat_root_file("testdata", "test_with_bussiness_time.csvy"))
  as_sql(x, "test.table")

})

