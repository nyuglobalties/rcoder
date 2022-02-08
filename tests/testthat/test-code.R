context("Coding")

test_that("code definitions are consistent", {
  expect_rc_error(code(1, 1))
  expect_rc_error(code(1, 1, description = 1))
  expect_rc_error(code(NA, 1))
  expect_rc_error(code(c("Bad", "Entry"), 1))
  expect_rc_error(code("Oops", 1:10))

  expect_identical(code("Yes", 1), code(value = 1, label = "Yes"))
})

test_that("coding creation makes sense", {
  expect_identical(coding(), structure(list(), class = "coding", labels = integer()))
  expect_rc_error(coding("Yes", 1))
  expect_rc_error(coding(code("Yes", "Yes"), code("No", 1)))
  expect_rc_error(coding(code("Yes", 1), code("Yes", 0)))
})

test_that("Safe coding evaluation works", {
  expr <- bquote(coding(code("Yes", 1), code("No", 2)))

  expect_identical(eval_coding(expr), coding(code("Yes", 1), code("No", 2)))
})

test_that("coding string representation works", {
  coding_chr <- "coding(code(\"Yes\", 1), code(\"No\", 0))"
  expect_identical(
    as.character(coding(code("Yes", 1), code("No", 0))),
    coding_chr
  )

  # Handling links_from deparsing
  coding_chr <- "coding(code(\"Yes\", 1), code(\"No\", 0), code(\"Not available\", NA, missing = TRUE, links_from = c(\"Missing\", \"Refused\")))" # nolint
  coding_chr1 <- "coding(code(\"Yes\", 1), code(\"No\", 0), code(\"Not available\", NA, missing = TRUE))" # nolint
  expect_identical(
    as.character(
      coding(code("Yes", 1), code("No", 0), code("Not available", NA, links_from = c("Missing", "Refused"))), # nolint
      include_links_from = TRUE
    ),
    coding_chr
  )

  # Ignore links_from by default
  expect_identical(
    as.character(coding(code("Yes", 1), code("No", 0), code("Not available", NA, links_from = c("Missing", "Refused")))), # nolint
    coding_chr1
  )
})