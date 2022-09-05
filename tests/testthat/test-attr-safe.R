test_that("Recoding is attribute safe", {
  x <- sample(0:1, 100, replace = TRUE)
  attr(x, "test") <- "test"

  y <- recode_vec(
    x,
    coding(code("Yes", "YES"), code("No", "NO")),
    from = coding(code("Yes", 1), code("No", 0))
  )

  expect_identical(
    attr(y, "test"),
    attr(x, "test")
  )
})
