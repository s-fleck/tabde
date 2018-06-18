context("value domains")


test_that("domains works as expected", {

  values(
    c("a", "a", "b", "b", "b"),
    c(1:2, 1:3)
  )


  expect_error(
    values(
      c("a", "a", "b", "b", "b"),
      c(1:2, c(1, 1, 2))
    ),
    "unique"
  )
})



test_that("", {
  vals <- values(
    c("letters", "letters", "numbers"),
    c("a", "b", 2)
  )

  td <- tabde(
    col_name   = c("Alpha", "Num"),
    col_type   = c("character", "numeric"),
    col_domain = c("letters", "numbers")
  )

  df <- data.frame(
    Alpha = c("a", "b", "c"),
    Num   = c(2, 2, 3),
    stringsAsFactors = FALSE
  )

  expect_true(matches_tabde(df, td))
  expect_false(matches_tabde(df, td, values = vals))
  expect_true(matches_tabde(df[1:2, ], td, values = vals))
})
