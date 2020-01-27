# Standard infix operators
`%||%` <- function(x, y) if (is.null(x)) y else x

`%==%` <- function(x, y) identical(x, y)
`%!=%` <- function(x, y) !identical(x, y)

# Aliases to common vapply calls
vlapply <- function(x, f, ...) vapply(x, f, logical(1L), ...)
viapply <- function(x, f, ...) vapply(x, f, integer(1L), ...)
vcapply <- function(x, f, ...) vapply(x, f, character(1L), ...)

# Positive integer checking.. good for allowing doubles for indexed situations
is_positive_integer <- function(x) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  isTRUE(all.equal(rep(0, length(x)), x %% 1)) &&
    all(x > 0)
}

ui_value <- function(x) {
  paste0("'", x, "'")
}

rc_err <- function(x, .envir = parent.frame()) {
  msg <- glue(glue_collapse(x), .envir = .envir)

  rlang::abort(.subclass = "rc_error", message = msg)
}

rc_assert <- function(x, msg = NULL, .envir = parent.frame()) {
  if (is.null(msg)) {
    deparsed <- deparse(substitute(x))
    msg <- glue("Assertion {ui_value(deparsed)} not met")
  } else {
    msg <- glue(glue_collapse(msg, "\n"), .envir = .envir)
  }

  if (!isTRUE(x)) {
    rc_err(msg)
  }

  invisible()
}
