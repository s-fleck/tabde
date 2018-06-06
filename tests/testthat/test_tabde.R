context("tabde")


test_that("tabde works as expected", {
  tres1 <- tabde(
    col_name = c("a", "b"),
    col_type = c("character", "numeric")
  )


  # Arguments must have the same length
  expect_error(
    tabde_fwf(
      col_name = c("a", "b"),
      col_type = c("numeric")
    )
  )

  expect_true("table_design" %in% class(tres1))
})




test_that("tabde_fwf works as expected", {
  tres <- tabde_fwf(
    col_name = c("a", "b"),
    col_type = c("character", "numeric"),
    fwf_start = c(1, 1),
    fwf_end = c(2, 2)
  )

  expect_true(identical(
    class(tres)[1:2],
    c("table_design_fwf", "table_design")
  ))
})






test_that("tabde_sql works as expected", {

  expect_error(tabde_sql(
    col_name = c("a", "b"),
    col_type = c("character", "numeric")
  ))


  tres <- tabde_sql(
    col_name = c("a", "b"),
    col_type = c("character", "numeric"),
    sql_type = c("varchar(255)", "integer")
  )

  tres <- tabde_sql(
    col_name = c("a", "b"),
    col_type = c("character", "numeric"),
    sql_type = c("varchar(255)", "integer"),
    sql_opts = c("NOT NULL", "")
  )

  expect_true(identical(
    class(tres)[1:2],
    c("table_design_sql", "table_design")
  ))

})
