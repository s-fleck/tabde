context("generate_sql")


test_that("generate_sql works as expected", {

  cn1 <- LETTERS[1:18]
  ct1 <- c("SMALLINT", "INTEGER", "INT", "BIGINT", "DECIMAL", "NUMERIC", "DECFLOAT",
           "REAL", "DOUBLE", "CHARACTER", "CHARACTER(1)", "VARCHAR(9)", "CLOB(1)",
           "GRAPHIC(80)", "VARGRAPHIC(80)", "DBCLOB(80)", "BLOB(80)", "FAIL")
  co1 <- c(rep("NOT NULL", length(ct1)))

  expect_silent(sql_create_table("testtable", cn1[1:3], ct1[1:3]))
  expect_error(
    sql_create_table("testtable", cn1[1:3], ct1[1:2]),
    "is_equal_length"
  )

  ct1[[1]] <- NA

  expect_message(
    sql_create_table("testtable", cn1[1:3], ct1[1:3]),
    "Skipping 1"
  )

  cn1[[1]] <- NA
  expect_error(
    sql_create_table("testtable", cn1[1:3], ct1[1:3]),
    "must be unique"
  )

  cn1[[1]] <- "B"
  expect_error(
    sql_create_table("testtable", cn1[1:3], ct1[1:3]),
    "must be unique"
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
    sql_create_table(
      "blah.table",
      col_names = x$col_name,
      col_types = x$sql_type,
      col_opts  = x$sql_opts
    )
  )
})




test_that("sql_create_table_cols works as expected", {
  expect_identical(sql_create_table_columns(
    c("blah", "blubb"),
    c("integer", "integer"),
    c("NOT NULL", "UNIQUE")
  ),
    c("blah INTEGER NOT NULL", "blubb INTEGER UNIQUE")
  )
})




test_that("sql_create_table_constraints works as expected for primary keys", {
  expect_identical(sql_create_table_constraints(
    c("X_BLAH_PK"),
    c("PRIMARY KEY"),
    list(c("firstname", "lastname"))
  ),
    "CONSTRAINT X_BLAH_PK PRIMARY KEY (firstname, lastname)"
  )
})




test_that("sql_create_table works with columns and primary keys", {
  sql_create_table(
    "test.table",
    col_names = c("blah", "blubb"),
    col_types = c("integer", "integer"),
    col_opts  = c("", "not null")
  )
})

