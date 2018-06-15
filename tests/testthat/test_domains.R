context("domains")


test_that("domains works as expected", {

  domains(
    c("a", "a", "b", "b", "b"),
    c(1:2, 1:3)
  )


  expect_error(
    domains(
      c("a", "a", "b", "b", "b"),
      c(1:2, c(1, 1, 2))
    ),
    "unique"
  )

})
