context("linking")

test_that("from only accepts a coding or list of codings", {
  coding_1 <- coding(
    code("Yes", 1),
    code("No", 0),
    code("Not present", -99),
    code("Refused", -88),
    code("Don't know", -77)
  )

  coding_2 <- coding(
    code("Yes", "YES"),
    code("No", "NO"),
    code("No response", "N/A")
  )

  coding_master <- coding(
    code("Yes", 1),
    code("No", 0),
    code(
      "Missing",
      NA,
      links_from = c(
        "Not present",
        "Refused",
        "Don't know",
        "No response"
      )
    )
  )

  expect_error(link_codings(coding_master, list("bad")))
  expect_error(link_codings(coding_master, "more bad"))

  test_tbl <- tibble::tribble(
    ~link, ~label_to, ~value_to, ~description_to, ~label_1, ~value_1, ~description_1,
    "Don't know", "Missing", NA, "Missing", "Don't know",  -77, "Don't know",
    "No", "No", 0, "No", "No", 0, "No",
    "Not present", "Missing", NA, "Missing", "Not present", -99, "Not present",
    "Refused", "Missing", NA, "Missing", "Refused", -88, "Refused",
    "Yes", "Yes", 1, "Yes", "Yes", 1, "Yes"
  )

  linked_c1 <- link_codings(coding_master, coding_1)

  expect_true(setequal(names(test_tbl), names(linked_c1)))
  expect_true(setequal(linked_c1, test_tbl))

  test_tbl_2 <- tibble::tribble(
    ~link, ~label_to, ~value_to, ~description_to, ~label_1, ~value_1, ~description_1, ~label_2, ~value_2, ~description_2,
    "Don't know", "Missing", NA, "Missing", "Don't know",  -77, "Don't know", NA, NA, NA,
    "No", "No", 0, "No", "No", 0, "No", "No", "NO", "No",
    "No response", "Missing", NA, "Missing", NA, NA, NA, "No response", "N/A", "No response",
    "Not present", "Missing", NA, "Missing", "Not present", -99, "Not present", NA, NA, NA,
    "Refused", "Missing", NA, "Missing", "Refused", -88, "Refused", NA, NA, NA,
    "Yes", "Yes", 1, "Yes", "Yes", 1, "Yes", "Yes", "YES", "Yes"
  )

  linked_all_indexed <- link_codings(coding_master, list(coding_1, coding_2))
  expect_true(setequal(names(test_tbl_2), names(linked_all_indexed)))
  expect_true(setequal(linked_all_indexed, test_tbl_2))
})
