context("linking")

test_that("No found links trigger an error", {
  coding_from <- coding(
    code("Yes", 1),
    code("No", 0)
  )

  coding_to <- coding(
    code("YES", "YES"),
    code("NO", "NO")
  )

  err <- expect_error(link_codings(coding_to, coding_from))
  expect_true(grepl("^Some links", err$message))

  err2 <- expect_error(link_codings(coding_to, coding_from, coding_from))
  expect_true(grepl("^Some links", err2$message))
})

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

  test_tbl <- dplyr::tribble(
    ~link, ~label_to, ~value_to, ~label_1, ~value_1,
    "Don't know", "Missing", NA, "Don't know", -77,
    "No", "No", 0, "No", 0,
    "No response", "Missing", NA, NA, NA,
    "Not present", "Missing", NA, "Not present", -99,
    "Refused", "Missing", NA, "Refused", -88,
    "Yes", "Yes", 1, "Yes", 1
  )

  linked_c1 <- link_codings(coding_master, coding_1)

  expect_true(setequal(names(test_tbl), names(linked_c1)))
  expect_true(setequal(linked_c1, test_tbl))

  test_tbl_2 <- dplyr::tribble(
    ~link, ~label_to, ~value_to, ~label_1, ~value_1, ~label_2, ~value_2,
    "Don't know", "Missing", NA, "Don't know", -77, NA, NA,
    "No", "No", 0, "No", 0, "No", "NO",
    "No response", "Missing", NA, NA, NA, "No response", "N/A",
    "Not present", "Missing", NA, "Not present", -99, NA, NA,
    "Refused", "Missing", NA, "Refused", -88, NA, NA,
    "Yes", "Yes", 1, "Yes", 1, "Yes", "YES"
  )

  linked_all_indexed <- link_codings(coding_master, list(coding_1, coding_2))
  expect_true(setequal(names(test_tbl_2), names(linked_all_indexed)))
  expect_true(setequal(linked_all_indexed, test_tbl_2))
})

test_that("Incomplete linking creates an informative error", {
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

  err <- expect_error(link_codings(coding_2, coding_1))

  expect_true(is.data.frame(err$to))
  expect_true(is.data.frame(err$from))

  # Issue encountered in a project
  homogenized_coding <- coding(
    code("Nearly everyday", 3),
    code("More than half the days", 2),
    code("A few days", 1),
    code("Not at all", 0)
  )

  wave_coding <- coding(
    code("Nearly everyday", 3),
    code("More than half the days", 2),
    code("A few days", 1),
    code("No days", 0)
  )

  err <- expect_rc_error(link_codings(homogenized_coding, wave_coding))

  expect_identical(err$missing_links, "No days")
})