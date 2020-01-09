# Standard infix operators
`%not_in%` <- function(x, y) !x %in% y

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
