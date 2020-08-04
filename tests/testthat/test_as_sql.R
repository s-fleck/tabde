context("as_sql")


test_that("generate_sql works as expected", {

  cn1 <- LETTERS[1:18]
  ct1 <- c("SMALLINT", "INTEGER", "INT", "BIGINT", "DECIMAL", "NUMERIC", "DECFLOAT",
           "REAL", "DOUBLE", "CHARACTER", "CHARACTER(1)", "VARCHAR(9)", "CLOB(1)",
           "GRAPHIC(80)", "VARGRAPHIC(80)", "DBCLOB(80)", "BLOB(80)", "FAIL")
  co1 <- c(rep("NOT NULL", length(ct1)))

  expect_silent(sql_create_table("testtable", cn1[1:3], ct1[1:3]))
  expect_error(
    sql_create_table("testtable", cn1[1:3], ct1[1:2]),
    class = "ValueError"
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
      col_name = x$col_name,
      col_type = x$sql_type,
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
  expect_identical(
    sql_create_table(
      "test.table",
      col_name = c("blah", "blubb"),
      col_type = c("integer", "integer"),
      col_opts  = c("", "not null"),
      const_name = "X_TEST_PK",
      const_type = "PRIMARY KEY",
      const_cols = list(c("blah", "blubb"))
    ),
    "CREATE TABLE test.table (blah INTEGER, blubb INTEGER not null, CONSTRAINT X_TEST_PK PRIMARY KEY (blah, blubb))"
  )


  expect_error(sql_create_table(
    "test.table",
    col_name = c("blah", "blubb"),
    col_type = c("integer", "integer"),
    col_opts  = c("", "not null"),
    const_name = "X_TEST_PK",
    const_type = "PRIMARY KEY",
    const_cols = list(c("blah", "blubb", "foo"))
  ), "foo")
})




test_that("as_sql.table_design_sql works as expected with constraints", {
  x <- tabde_sql(
    col_name = c("blah", "blubb"),
    col_type = c("integer", "integer"),
    sql_opts  = c("", "not null"),
    sql_type = c("integer", "integer"),
    .constraints = tabde_constraints(
      const_name = "X_TEST_PK",
      const_type = "PRIMARY KEY",
      const_cols = list(c("blah", "blubb"))
    )
  )

  expect_identical(
    as_sql(x, "test.table"),
    sql_create_table(
      "test.table",
      col_name = c("blah", "blubb"),
      col_type = c("integer", "integer"),
      col_opts  = c("", "not null"),
      const_name = "X_TEST_PK",
      const_type = "PRIMARY KEY",
      const_cols = list(c("blah", "blubb"))
    )
  )
})



test_that("sql_create_table_columns fails gracefully", {
  expect_error(sql_create_table_columns(1, "BLAH", "BLAH"), class = "TypeError")
  expect_error(sql_create_table_columns("BLAH", "BLAH", c("BL", "AH")), class = "ValueError")
})
