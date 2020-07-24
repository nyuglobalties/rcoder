#' Evaluate a collection of codings from a character vector
#'
#' @param x A character vector
#'
#' @return A list of codings
#' @export
#'
#' @examples
#' char_vec <- c("coding(code('Yes', 1), code('No', 0))", "")
#' as_coding_list(char_vec)
as_coding_list <- function(x) {
  rc_assert(is.character(x) || is.logical(x))

  structure(
    lapply(x, coding_list_element),
    class = c("coding_lst", "list")
  )
}

coding_list_element <- function(x) {
  rc_assert(length(x) == 1)

  if (is.na(x) || x == "") {
    return(empty_coding())
  }

  eval_coding(rlang::parse_expr(x))
}
