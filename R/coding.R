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

labels.coding <- function(x, ...) attr(x, "labels", exact = TRUE)

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

  dfs <- lapply(coding, as.data.frame)
  content <- dplyr::bind_rows(!!!dfs)
  content[["link"]] <- content[["links_from"]]
  content[["links_from"]] <- NULL

  content <- content[, c("link", setdiff(names(content), "link"))]
  content
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
  out <- coding_contents(x)

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

  print(dplyr::as_tibble(as.data.frame(x)))

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
