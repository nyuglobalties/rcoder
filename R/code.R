#' Encode a label to a value with other metadata
#'
#' The most fundamental components of a `code` object are the `label` and
#' `value` elements. A `code` object is essentially a key-value tuple that has
#' some extra metadata.
#'
#' @param label A short label for a value in a vector
#' @param value The raw value found in the respective vector
#' @param description A longer-form label for the value, if extra context for
#'   that label is needed
#' @param links_from A reference to another `code` in another `coding` object
#'   for recoding purposes.
#' @param missing Whether this `code` represents a missing response
#' @param ... Any extra metadata
#' @return A `code` object that contains the key-value map of label to value
#'
#' @export
#' @examples
#' code("Yes", 1)
#' code("No", 0)
#' code(
#'   "No response", -88,
#'   description = "Participant ignored question when prompted",
#'   missing = TRUE
#' )
#' code("Missing", NA, links_from = c("Refused", "Absent"))
code <- function(label,
                 value,
                 description = label,
                 links_from = label,
                 missing = FALSE,
                 ...) {
  if (missing(value)) {
    rc_err(c(
      "No value provided to label {ui_value(label)}. ",
      "Probably caused by forgetting the second argument to code()"
    ))
  }

  rc_assert(
    is.character(label),
    "label ({ui_value(label)}) has type {ui_value(typeof(label))} when it should be 'character'."
  )

  rc_assert(
    is.character(description),
    "description ({ui_value(description)}) has type {ui_value(typeof(description))} when it should be 'character'."
  )

  rc_assert(
    is.character(links_from),
    "links_from ({ui_value(links_from)}) has type {ui_value(typeof(links_from))} when it should be 'character'."
  )

  check_code_entry_length(value, "codes")
  check_code_entry_length(label, "labels")
  check_code_entry_length(description, "descriptions")

  if (all(is.na(label))) {
    rc_err("A code cannot have a missing label.")
  }

  if (!isTRUE(missing) && is.na(value)) {
    missing <- TRUE
  }

  structure(
    list(
      label = label,
      value = value,
      description = description,
      links_from = links_from,
      missing = missing,
      ...
    ),
    class = "code"
  )
}

#' @export
print.code <- function(x, ...) {
  cat_line("<Code>")
  print(glue("label: {ui_value(x$label)}"))
  print(glue("value: {x$value}"))

  if (!identical(x$description, x$label)) {
    print(glue("description: {ui_value(x$description)}"))
  }

  if (!identical(x$links_from, x$label)) {
    print(glue("linking from: [{glue_collapse(ui_value(x$links_from), ', ')}]"))
  }

  if (isTRUE(x$missing)) {
    print(glue("Represents a missing value"))
  }

  invisible()
}

check_code_entry_length <- function(x, type) {
  if (length(x) > 1) {
    rc_err(c(
      "Cannot have multivalued {type}.\n",
      "code() is specifically for the meaning of each value ",
      "in a variable."
    ))
  }

  invisible(x)
}

#' @export
as.data.frame.code <- function(x, ...) {
  for (el in names(x)[names(x) != "links_from"]) {
    x[[el]] <- rep(x[[el]], length(x$links_from))
  }

  as.data.frame(dplyr::as_tibble(c(x)))
}

