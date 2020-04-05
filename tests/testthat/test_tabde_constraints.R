context("tabde_constraints")


test_that("tabde_constraints works as expected", {
  expect_true(nrow(tabde_constraints("blubb", "primary key", "foo")) == 1L)
  expect_true(nrow(tabde_constraints("blubb", "primary key", list(c("foo", "bar")))) == 1L)
})



test_that("tabde_constraints works as expected", {

  yaml <-
'
---
constraints:
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
  x <- x[["constraints"]]
  res <- as_tabde_constraints(x)

  expect_true(all(res$const_type == "PRIMARY KEY"))
  expect_identical(
    res$const_cols,
    I(list("name", c("first_name", "last_name")))
  )
})
