context("sql_header")


test_that("sql_header works as expected", {
  expect_true(nrow(sql_header("blubb", "primary key", "foo")) == 1L)
  expect_true(nrow(sql_header("blubb", "primary key", list(c("foo", "bar")))) == 1L)
})



test_that("sql_header works as expected", {

yaml <-
'
---
constraint:
  X_NAME_PK:
    type: "PRIMARY KEY"
    columns: name
  X_FULLNAME_PK:
    type: "PRIMARY KEY"
    columns:
      - first_name
      - last_name

---
'
  x <- yaml::read_yaml(text = yaml)
  res <- as_sql_header(x)

  expect_true(all(res$const_type == "PRIMARY KEY"))
  expect_identical(
    res$const_cols,
    I(list("name", c("first_name", "last_name")))
  )
})




test_that("as_sql_header.data.frame works as expected", {

  df <- data.frame(
    const_name = "foo",
    const_class = "constraint",
    const_type = "primary key",
    const_cols = "id",
    metainfo = "foobar",
    othercol = "blah"
  )

  expect_length(as_sql_header(df), 6)
  expect_s3_class(as_sql_header(df), "sql_header")
})
