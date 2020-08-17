test_that("ODK conversion works", {
  foo <- coding(code("Yes", 1), code("No", 0))

  # Coding label must be specified
  expect_error(coding_to_odk(foo))

  foo <- coding(code("Yes", 1), code("No", 0), .label = "yesno")

  checkfoo <- dplyr::tribble(
    ~list_name, ~name, ~label,
    "yesno", 1, "Yes",
    "yesno", 0, "No"
  )

  expect_equivalent(coding_to_odk(foo), checkfoo)

  bar <- coding(
    code("Yes", 1),
    code("No", 0),
    code(
      "Missing",
      -99,
      links_from = c(
        "No response",
        "Refused",
        "Absent"
      )
    ),
    .label = "yesno"
  )

  checkbar <- dplyr::tribble(
    ~list_name, ~name, ~label,
    "yesno", 1, "Yes",
    "yesno", 0, "No",
    "yesno", -99, "Missing"
  )

  expect_equivalent(coding_to_odk(bar), checkbar)
})

test_that("Haven labels are generated correctly", {
  foo <- coding(code("Yes", 1), code("No", 0))
  bar <- coding(
    code("Yes", 1),
    code("No", 0),
    code(
      "Missing",
      -99,
      links_from = c(
        "No response",
        "Refused",
        "Absent"
      )
    )
  )

  checkfoo <- c(Yes = 1, No = 0)
  checkbar <- c(Yes = 1, No = 0, Missing = -99)

  expect_identical(coding_to_haven_labels(foo), checkfoo)
  expect_identical(coding_to_haven_labels(bar), checkbar)
})
