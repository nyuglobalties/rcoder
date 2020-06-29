#' Checks if vector's content adheres to a coding
#'
#' Performs to check to see if the set of vector values are equal to or a subset
#' of a coding's values.
#'
#' @param vec A vector
#' @param coding A `coding` object
#' @param ignore_empty Logical flag to skip check if coding is empty
#' @return TRUE/FALSE
#'
#' @export
matches_coding <- function(vec, coding, ignore_empty = TRUE) {
  rc_assert(is.coding(coding), "{ui_value(substitute(coding))} is not a coding object.")

  if (!is.vector(vec)) {
    return(FALSE)
  }

  if (is_empty_coding(coding) && isTRUE(ignore_empty)) {
    return(TRUE)
  }

  all(unique(vec) %in% coding_values(coding))
}

#' @describeIn matches_coding Rather than returning TRUE/FALSE, this function
#'   halts execution if `matches_coding()` returns FALSE.
#' @export
verify_matches_coding <- function(vec, coding, ignore_empty = TRUE) {
  if (!matches_coding(vec, coding, ignore_empty = ignore_empty)) {
    rc_err(c(
      "Content does not match coding.\n",
      "Content: [{paste0(unique(vec), collapse = ', ')}]\n",
      "Coding values: [{paste0(coding_values(coding), collapse = ', ')}]"
    ))
  }

  coding
}
