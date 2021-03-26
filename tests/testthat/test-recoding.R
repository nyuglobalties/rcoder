test_that("Recoding interface is consistent", {
  coding_1 <- coding(
    code("Yes", 1),
    code("No", 0),
    code("Not present", -99),
    code("Refused", -88),
    code("Don't know", -77)
  )

  coding_2 <- coding(
    code("Yes", 1),
    code("No", 0),
    code(
      "No response",
      NA,
      links_from = c(
        "Not present",
        "Refused",
        "Don't know"
      )
    )
  )

  linked <- link_codings(coding_2, coding_1)

  rcf_1 <- make_recode_query(linked)
  rcf_2 <- make_recode_query(linked, 1)

  expect_equivalent(rcf_1, rcf_2)
})

test_that("Recoding functionality works", {
  coding_1 <- coding(
    code("Yes", 1),
    code("No", 0),
    code("Not present", -99),
    code("Refused", -88),
    code("Don't know", -77)
  )

  coding_2 <- coding(
    code("Yes", 1),
    code("No", 0),
    code(
      "No response",
      NA,
      links_from = c(
        "Not present",
        "Refused",
        "Don't know"
      )
    )
  )

  linked <- link_codings(coding_2, coding_1)
  rcf <- make_recode_query(linked)

  set.seed(92958)
  vec <- sample(c(0, 1, -99, -88, -77), 100, replace = TRUE)
  vec_correct <- ifelse(vec %in% c(-99, -88, -77), NA, vec)

  expect_equivalent(rcf(vec), vec_correct)

  # Since all `to` codes are integers, confirm that rcoder converted
  # to integers, not doubles.
  expect_true(is.integer(rcf(vec)))
})

test_that("Vector recoding works", {
  set.seed(9001)

  vec <- sample(0:3, 20, replace = TRUE)
  expect_null(get_attr(vec, "rcoder.coding"))

  coding_1 <- coding(
    code("Never", 0),
    code("Rarely", 1),
    code("Sometimes", 2),
    code("Frequently", 3)
  )

  expect_error(recode_vec(vec, to = coding_1))

  vec <- assign_coding(vec, coding_1)
  expect_true(!is.null(get_attr(vec, "rcoder.coding")))
  expect_identical(get_attr(vec, "rcoder.coding"), coding_1)
  expect_identical(
    get_attr(vec, "bpr.coding"),
    as.character(coding_1)
  )

  coding_2 <- coding(
    # Using 10 & 11 for no common value overlap
    code("Uncommon", 10, links_from = c("Never", "Rarely")),
    code("Common", 11, links_from = c("Sometimes", "Frequently"))
  )

  vec <- recode_vec(vec, to = coding_2, .bpr = FALSE)
  expect_identical(get_attr(vec, "rcoder.coding"), coding_2)
  expect_null(get_attr(vec, "bpr.coding"))
  expect_true(all(vec %in% c(10, 11)))
})