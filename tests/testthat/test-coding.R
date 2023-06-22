test_that("Coding selection works correctly", {
  f <- function(x) isTRUE(x$missing)

  test_c <- coding(code("Yes", 1), code("No", 0), code("Missing", NA))

  res <- select_codes_if(test_c, f)
  expect_true(length(res) == 1)
  expect_true(inherits(res, "coding"))
  expect_equivalent(res, coding(code("Missing", NA)))
})

test_that("Long codings get deparsed correctly", {
  cdng <- lapply(1:1000, function(i) {
    text <- paste0(
      "Very long code ", i,
      "that has tens of bytes in it which are unnecessary. ",
      "This code text is insanely long and rarely useful."
    )

    code(text, text)
  })

  cdng <- do.call(coding, cdng)
  cdng_chr <- as.character(cdng)

  expect_true(length(cdng_chr) == 1L)
  expect_true(nchar(cdng_chr) > 50000L)
})
