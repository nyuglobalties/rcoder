#' @export
matches_content <- function(coding, vec, ignore_empty = TRUE) {
    rc_assert(is.coding(coding), "{ui_value(substitute(coding))} is not a coding object.")

    if (!is.vector(vec)) {
        return(FALSE)
    }

    if (is_empty_coding(coding) && isTRUE(ignore_empty)) {
        return(TRUE)
    }

    setequal(coding_values(coding), vec)
}

#' @export
verify_matches_content <- function(coding, vec, ...) {
    if (!matches_content(coding, vec, ...)) {
        rc_err(c(
            "Content does not match coding.\n",
            "Content: [{paste0(unique(vec), collapse = ', ')}]\n",
            "Coding values: [{paste0(coding_values(coding), collapse = ', ')}]"
        ))
    }

    coding
}
