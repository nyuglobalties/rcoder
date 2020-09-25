#' Convert a coding object to ODK XLSForm choices
#'
#' ODK XLSForms link the categorical codings to a variable type name in the
#' 'survey' sheet. The codings are specified in the 'choices' sheet which has
#' a `list_name` column that holds the variable type names. Each row that has
#' that name will be associated with that categorical type coding. This function
#' converts `coding` objects into tables that can be inserted into that
#' 'choices' sheet. The categorical type is specified with the coding `.label`.
#'
#' @param coding A coding object
#' @return A data.frame or tibble that can be included in an XLSForm 'choices'
#'         sheet
#' @export
coding_to_odk <- function(coding) {
  rc_assert(is.coding(coding))

  if (is_empty_coding(coding)) {
    return(NULL)
  }

  if (is.null(coding_label(coding))) {
    rc_err(c(
      "Coding label must be specified for ODK XLSForm choices.\n",
      "The label is used for the `list_name` column in the choices tab.\n",
      "Set the label with the `.label` parameter in the `coding()` call."
    ))
  }

  contents <- coding_contents(coding)

  dplyr::tibble(
    list_name = coding_label(coding),
    name = unique(contents$value),
    label = unique(contents$label)
  )
}

#' Convert coding to `haven`-compatible labels
#'
#' Converts a `coding` object into a named vector to be used in the `labels`
#' parameter for `haven::labelled()`.
#'
#' @param coding A coding object
#' @return A named vector representation of the coding
#' @export
coding_to_haven_labels <- function(coding) {
  rc_assert(is.coding(coding))

  if (is_empty_coding(coding)) {
    return(NULL)
  }

  contents <- coding_contents(coding)
  h_labels <- unique(contents$value)
  nm_h_labels <- unique(contents$label)

  if (length(h_labels) != length(nm_h_labels)) {
    rc_err("Labels {ui_vec(nm_h_labels)} don't match {ui_vec(h_labels)}")
  }

  names(h_labels) <- unique(contents$label)

  h_labels
}
