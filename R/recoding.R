#' Make a recoding call from linked codings
#'
#' This creates a function that accepts a vector and recodes it from the
#' information provided in a `linked_coding_df`
#'
#' @param linked_codings A `linked_coding_df`
#' @param from A character or integer that selects the correct original coding.
#'             Defaults to 1, the first linked coding.
#' @param to_suffix The suffix used to signify which columns refer to values to
#'   which the vector will be recoded
#' @param ... Any other parameters passed onto the recoding function selector
#'
#' @export
make_recode_query <- function(linked_codings, from = 1, to_suffix = "to", ...) {
  stopifnot(inherits(linked_codings, "linked_coding_df"))

  suffixed_columns <- c("label", "value")
  from_column_patterns <- paste0(
    paste0(suffixed_columns, "_", from),
    collapse = "|"
  )

  to_column_patterns <- paste0(
    paste0(suffixed_columns, "_", to_suffix),
    collapse = "|"
  )

  from_columns <- grep(from_column_patterns, names(linked_codings), value = TRUE)
  to_columns <- grep(to_column_patterns, names(linked_codings), value = TRUE)

  if (length(from_columns) < 1) {
    rc_err("Origin '{from}' not found in linked coding data.frame.")
  }

  if (length(to_columns) < 1) {
    rc_err(c(
      "'to' columns for linked coding data.frame not found.\n",
      "Perhaps '{to_suffix}' is not the 'to' column ",
      "tag you chose?"
    ))
  }

  subset <- unique(linked_codings[, c("link", to_columns, from_columns)])

  # If wave label is NA, that value was not included in that wave. Drop.
  subset <- subset[!is.na(subset[[paste0("label_", from)]]), ]

  from_value <- paste0("value_", from)
  to_value <- paste0("value_", to_suffix)

  # End-user ease: if to codes are int-like, use integers instead.
  # Usually codes are integers, so it makes more sense to use that
  # storage mode instead.
  if (is_intlike(subset[[to_value]])) {
    subset[[to_value]] <- as.integer(subset[[to_value]])
  }

  if (length(subset[[from_value]]) < 1) {
    print("linked_coding:")
    print(linked_codings)
    print("subset:")
    print(subset)
    rc_err(c(
      "A problem has occurred. ",
      "Contact the developer with the provided ",
      "'linked_codings' and 'subset' values."
    ))
  }

  recode_function(subset[[from_value]], subset[[to_value]], ...)
}

#' Recode a vector
#' 
#' A simple interface to recoding a vector based on the coding linking
#' mechanism. If the vector has the "rcoder.coding" attribute, then
#' the coding object stored in that attribute will be used by default.
#' 
#' @param vec A vector
#' @param to A coding object to which the vector will be recoded
#' @param from A coding object that describes the current coding
#'   of the vector. Defaults to the "rcoder.coding" attribute value
#' @param .embed If `TRUE`, `from` will be stored in the "rcoder.coding"
#'   attribute
#' @return The recoded vector
#' @export 
recode_vec <- function(
  vec,
  to,
  from = get_attr(vec, "rcoder.coding"),
  .embed = TRUE
) {
  if (is.null(from)) {
    rc_err("Use `rcoder::assign_coding` to embed a `coding` to a vector")
  }

  rc_assert(is.vector(vec), "{substitute(vec)} must be a vector")
  rc_assert(is.coding(to), "{substitute(to)} is not a `coding` object")
  rc_assert(
    is.coding(from),
    "{substitute(from)} is not a `coding`"
  )


  linked <- link_codings(to, from)
  recode_func <- make_recode_query(linked)

  recoded_vec <- recode_func(vec)

  if (isTRUE(.embed)) {
    recoded_vec <- assign_coding(recoded_vec, to)
  }

  recoded_vec
}

#' Adds a coding as an attribute to a vector
#' 
#' Stores a coding at the "rcoder.coding" attribute of a vector
#' 
#' @param vec A vector
#' @param .coding A `coding` object
#' @return The vector with its "rcoder.coding" attribute set to `.coding`
#' @export 
assign_coding <- function(vec, .coding) {
  rc_assert(is.vector(vec), "{substitute(vec)} must be a vector")
  rc_assert(is.coding(.coding), "{substitute(.coding)} must be a `coding`")

  set_attrs(vec, rcoder.coding = .coding)
}