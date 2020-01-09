context("Coding")

test_that("code definitions are consistent", {
    expect_error(code(1, 1))
    expect_error(code(1, 1, description = 1))
    expect_error(code(NA, 1))
    expect_error(code(c("Bad", "Entry"), 1))
    expect_error(code("Oops", 1:10))

    expect_identical(code("Yes", 1), code(value = 1, label = "Yes"))
})

test_that("coding creation makes sense", {
    expect_error(coding())
    expect_error(coding("Yes", 1))
    expect_error(coding(code("Yes", "Yes"), code("No", 1)))
    expect_error(coding(code("Yes", 1), code("Yes", 0)))
})
