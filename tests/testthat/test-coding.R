test_that("Coding selection works correctly", {
  f <- function(x) isTRUE(x$missing)

  test_c <- coding(code("Yes", 1), code("No", 0), code("Missing", NA))

  res <- select_codes_if(test_c, f)
  expect_true(length(res) == 1)
  expect_true(inherits(res, "coding"))
  expect_equivalent(res, coding(code("Missing", NA)))
})
