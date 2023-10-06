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
#' @seealso [odk_to_coding()]
#' @export
#' @examples
#' cdng <- coding(code("Yes", 1), code("No", 0), .label = "yesno")
#' coding_to_odk(cdng)
coding_to_odk <- function(coding) {
  rc_assert(is_coding(coding))

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

#' Convert ODK choices to a coding
#'
#' ODK XLSForms link the categorical codings to a variable type name in the
#' 'survey' sheet. The codings are specified in the 'choices' sheet which has
#' a `list_name` column that holds the variable type names. Each row that has
#' that name will be associated with that categorical type coding. This function
#' converts subsets of the choices sheet into individual 'coding' objects.
#'
#' @param choice_table A data.frame slice of the "choices" table from an
#'   XLSForm
#' @return A `coding` object that corresponds to the choices' slice
#' @seealso [coding_to_odk()]
#' @export
#' @examples
#' choice_excerpt <- data.frame(
#'   list_name = rep("yesno", 2),
#'   name = c("Yes", "No"),
#'   label = c(1, 0)
#' )
#'
#' odk_to_coding(choice_excerpt)
odk_to_coding <- function(choice_table) {
  ct_sym <- deparse1(substitute(choice_table)) # nolint

  rc_assert(
    is.data.frame(choice_table),
    msg = "{ui_value(ct_sym)} must be a data.frame"
  )

  expected_cols <- c("list_name", "name", "label")

  for (ec in expected_cols) {
    if (!ec %in% names(choice_table)) {
      rc_err("Expected column `{ec}` not found in {ui_value(ct_sym)}")
    }
  }

  rc_assert(
    length(unique(choice_table[["list_name"]])) == 1,
    msg = "`{ui_value(ct_sym)}` has multiple `list_name` values"
  )

  choice_table <- unique(choice_table)
  codes <- lapply(seq_len(nrow(choice_table)), function(i) {
    code(label = choice_table[["name"]][i], value = choice_table[["label"]][i])
  })

  do.call(coding, codes)
}

#' Convert coding to `haven`-compatible labels
#'
#' Converts a `coding` object into a named vector to be used in the `labels`
#' parameter for `haven::labelled()`.
#'
#' @param coding A coding object
#' @return A named vector representation of the coding
#' @export
#' @examples
#' cdng <- coding(code("Yes", 1), code("No", 0))
#' coding_to_haven_labels(cdng)
coding_to_haven_labels <- function(coding) {
  rc_assert(is_coding(coding))

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
