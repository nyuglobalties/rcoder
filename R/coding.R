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
#'
#' @export
code <- function(label,
                 value,
                 description = label,
                 links_from = label,
                 missing = FALSE,
                 ...) {
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

#' Catalog the categorical data representation in a vector
#'
#' A `coding` object holds a list of `code`s that map vector values to human
#' readable labels. An abstraction of factors, this data structure is designed
#' to be portable and not directly attached to the underlying data. Moreover,
#' `coding` objects can be "linked" for recoding and data lineage purposes. An
#' "empty coding" is used to represent data that has no categorical
#' interpretation.
#'
#' @param ... A collection of `code` objects
#' @param .label A label for this coding, available for interoperability
#'
#' @export
coding <- function(..., .label = NULL) {
  if (missing(..1)) {
    return(empty_coding())
  }

  codes <- list(...)

  if (!all(vlapply(codes, function(x) inherits(x, "code")))) {
    rc_err("coding() only accepts code objects as arguments.")
  }

  loc_labels <- lapply(seq_along(codes), function(i) list(index = i, label = codes[[i]]$label))
  labels <- vcapply(loc_labels, function(x) x$label)

  if (any(duplicated(labels))) {
    rc_err("Multiple labels set in a single coding. Each label must be unique.")
  }

  values <- lapply(codes, function(x) x$value)
  values_no_na <- values[!is.na(values)]

  if (!all(vcapply(values_no_na, typeof) == typeof(values_no_na[[1]]))) {
    rc_err(c(
      "Coding types must be constant.\n",
      "Perhaps you forgot to wrap your numbers in quotes?"
    ))
  }

  locations <- viapply(loc_labels, function(x) x$index)
  names(locations) <- labels

  structure(
    codes,
    class = "coding",
    labels = locations,
    label = .label
  )
}

#' @rdname coding
#' @export
empty_coding <- function() {
  structure(
    list(),
    class = "coding",
    labels = integer()
  )
}

#' Is an object the empty coding?
#'
#' @param x An object
#' @return TRUE/FALSE if the object is identical to `empty_coding()`
#' @export
is_empty_coding <- function(x) {
  identical(x, empty_coding())
}

is.coding <- function(x) inherits(x, "coding")

labels.coding <- function(x, ...) attr(x, "labels")

select_codes_if <- function(.coding, .p, ...) {
  rc_assert(is.coding(.coding) && is.function(.p))

  matching_codes <- lapply(.coding, function(code) {
    if (.p(code, ...)) {
      code
    } else {
      list()
    }
  })

  matching_codes <- matching_codes[!vlapply(matching_codes, function(x) identical(x, list()))]

  do.call(coding, matching_codes)
}

select_codes_by_label <- function(.coding, .labels) {
  rc_assert(is.character(.labels))

  select_codes_if(.coding, function(code) code$label %in% .labels)
}

coding_values <- function(coding) {
  stopifnot(is.coding(coding))

  if (is_empty_coding(coding)) {
    return(logical())
  }

  unlist(lapply(coding, function(code) code$value))
}

missing_codes <- function(coding) {
  rc_assert(is.coding(coding))

  if (is_empty_coding(coding)) {
    return(coding)
  }

  select_codes_if(coding, function(code) isTRUE(code$missing))
}

coding_contents <- function(coding) {
  if (is_empty_coding(coding)) {
    return(list(
      links = character(),
      labels = character(),
      values = logical(), # To reflect total absence of data
      descriptions = character()
    ))
  }

  coding <- lapply(coding, function(x) {
    x$label <- rep(x$label, length(x$links_from))
    x$value <- rep(x$value, length(x$links_from))
    x$description <- rep(x$description, length(x$links_from))

    x
  })

  list(
    links = unlist(lapply(coding, function(x) x$links_from)),
    labels = unlist(lapply(coding, function(x) x$label)),
    values = unlist(lapply(coding, function(x) x$value)),
    descriptions = unlist(lapply(coding, function(x) x$description))
  )
}

coding_label <- function(coding) {
  attr(coding, "label", exact = TRUE)
}

#' @export
as.data.frame.coding <- function(
  x,
  row.names = NULL,
  optional = NULL,
  suffix = NULL,
  ...
) {
  contents <- coding_contents(x)

  out <- data.frame(
    link = contents$links,
    label = contents$labels,
    value = contents$values,
    description = contents$descriptions,
    stringsAsFactors = FALSE
  )

  if (!is.null(suffix)) {
    stopifnot(is.character(suffix) || is_positive_integer(suffix))
    names(out) <- ifelse(names(out) == "link", names(out), paste0(names(out), "_", suffix))
  }

  out
}

#' @export
print.coding <- function(x, ...) {
  if (is_empty_coding(x)) {
    cat_line("<Empty Coding>")

    return(invisible())
  }

  if (!is.null(coding_label(x))) {
    cat_line("<Coding: '{coding_label(coding)}'>")
  } else {
    cat_line("<Coding>")
  }

  if (requireNamespace("tibble", quietly = TRUE)) {
    print(tibble::as_tibble(as.data.frame(x)))
  } else {
    print(as.data.frame(x))
  }

  invisible()
}

#' Evaluates a coding expression in a safe environment
#'
#' To prevent requiring attaching the `rcoder` package, this function takes in
#' an unevaluated expression -- assumed to be a `coding()` call -- and evaluates
#' the expression with _only_ `coding` and `code` provided to guard against
#' rogue code.
#'
#' @param expr An expression
#' @return An evaluated `coding` object
#' @export
eval_coding <- function(expr) {
  rc_assert(rlang::is_expression(expr))

  safe_env <- rlang::new_environment(parent = parent.frame())
  rlang::env_bind(safe_env, code = code, coding = coding)

  rlang::eval_bare(expr, env = safe_env)
}
