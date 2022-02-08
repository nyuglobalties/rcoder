expect_rc_error <- function(object, ...) {
  expect_error(object, class = "rc_error", ...)
}