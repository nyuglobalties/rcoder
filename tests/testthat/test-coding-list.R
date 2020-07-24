context("Coding lists")

test_that("Coding lists are evaluated correctly", {
  char_vec <- c("coding(code('Yes', 1), code('No', 0))", "")

  coding_list <- as_coding_list(char_vec)
  coding_list_ans <- structure(
    list(
      coding(code("Yes", 1), code("No", 0)),
      empty_coding()
    ),
    class = c("coding_lst", "list")
  )

  expect_identical(coding_list, coding_list_ans)

  lgl_vec <- c(NA, NA)

  coding_list <- as_coding_list(lgl_vec)
  coding_list_ans <- structure(
    list(
      empty_coding(),
      empty_coding()
    ),
    class = c("coding_lst", "list")
  )

  expect_identical(coding_list, coding_list_ans)
})
