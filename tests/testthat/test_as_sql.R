context("generate_sql")


test_that("generate_sql works as expected", {

  cn1 <- LETTERS[1:18]
  ct1 <- c("SMALLINT", "INTEGER", "INT", "BIGINT", "DECIMAL", "NUMERIC", "DECFLOAT",
           "REAL", "DOUBLE", "CHARACTER", "CHARACTER(1)", "VARCHAR(9)", "CLOB(1)",
           "GRAPHIC(80)", "VARGRAPHIC(80)", "DBCLOB(80)", "BLOB(80)", "FAIL")
  co1 <- c(rep("NOT NULL", length(ct1)))

  expect_silent(generate_sql_create_table("testtable", cn1[1:3], ct1[1:3]))
  expect_error(
    generate_sql_create_table("testtable", cn1[1:3], ct1[1:2]),
    "is_equal_length"
  )


  ct1[[1]] <- NA

  expect_warning(
    generate_sql_create_table("testtable", cn1[1:3], ct1[1:3]),
    "Skipping 1"
  )
})




test_that("as_sql works as expected", {

  x <- tabde_sql(
    col_name = c("blah", "blubb"),
    col_type = c("integer", "integer"),
    sql_type = c("integer", "integer")
  )


  expect_identical(
    as_sql(x, "blah.table"),
    generate_sql_create_table(
      "blah.table",
      col_names = x$col_name,
      col_types = x$sql_type,
      sql_opts  = x$sql_opts
    )
  )
})
