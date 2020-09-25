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
