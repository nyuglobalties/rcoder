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

ui_vec <- function(x) {
  if (is.character(x)) {
    chr_x <- paste0("'", x, "'")
  } else {
    chr_x <- as.character(x)
  }

  paste0("[", paste0(chr_x, collapse = ", "), "]")
}

cat_line <- function(x, .envir = parent.frame()) {
  cat(glue(x, .envir = .envir), "\n", sep = "")
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

is_intlike <- function(x) {
  if (!is.numeric(x)) {
    return(FALSE)
  }

  if (all(is.na(x))) {
    return(TRUE)
  }

  x_nona <- x[!is.na(x)]

  isTRUE(all.equal(rep(0, length(x_nona)), x_nona %% 1))
}

get_attr <- function(obj, attrib) {
  attr(obj, attrib, exact = TRUE)
}

set_attrs <- function(obj, ...) {
  dots <- rlang::dots_list(...)

  if (is.null(names(dots)) || any(names(dots) == "")) {
    stop0("All attribs must have names")
  }

  for (d in names(dots)) {
    attr(obj, d) <- dots[[d]]
  }

  obj
}