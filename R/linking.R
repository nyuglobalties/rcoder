#' Link a coding from others for recoding
#'
#' Coding objects can be linked together to create mappings from one or more
#' codings to another. This creates a `data.frame` that outlines how the codings
#' are linked, to be used in `make_recode_query()`.
#'
#' @param to A coding to be linked to
#' @param ... Codings to be linked from
#' @param .to_suffix A suffix signifying which columns in the output `data.frame`
#'   came from `to`
#' @param .drop_unused Logical flag to drop any codes in `to` that have no
#'   counterparts in `...`
#' @return A `linked_coding_df` with all necessary information for a recoding
#'   query
#'
#' @export
link_codings <- function(to, ..., .to_suffix = "to", .drop_unused = TRUE) {
  rc_assert(is.coding(to))

  from <- rlang::dots_list(...)

  if (length(from) == 1) {
    from <- from[[1]]
  }

  if (!is.coding(from)) {
    if (!is.list(from)) {
      rc_err("`...` must be a coding or codings.")
    }

    if (!all(vlapply(from, is.coding))) {
      rc_err("Not all of `...` is a coding object.")
    }
  }

  if (!is.coding(from)) {
    from_dat <- coding_list_to_df(from)
  } else {
    from_dat <- as.data.frame(from, suffix = 1)
  }

  to_dat <- as.data.frame(to, suffix = .to_suffix)

  if (nrow(to_dat) < nrow(from_dat)) {
    rc_err("Not all cases covered while linking codings.")
  }

  if (isTRUE(.drop_unused)) {
    to_dat <- drop_unused_links(to_dat, from_dat)
  }

  # Only include link, value, and label
  filter_pattern <- paste0(paste0("^", c("link", "value", "label")), collapse = "|")
  to_dat <- to_dat[, grepl(filter_pattern, names(to_dat))]
  from_dat <- from_dat[, grepl(filter_pattern, names(from_dat))]

  dat <- merge(to_dat, from_dat, by = "link", all.x = TRUE)
  class(dat) <- c(class(dat), "linked_coding_df")

  dat
}

coding_list_to_df <- function(coding_list) {
  suffixes <- if (!is.null(names(coding_list))) {
    # Assumed to be the wave tags
    names(coding_list)
  } else {
    1:length(coding_list)
  }

  mapped <- Map(
    function(.x, .y) as.data.frame(.x, suffix = .y),
    coding_list,
    suffixes
  )

  if (length(mapped) > 1) {
    Reduce(function(x, y) merge(x, y, by = "link", all = TRUE), mapped)
  } else {
    mapped[[1]]
  }
}

drop_unused_links <- function(to_dat, from_dat) {
  from_links <- from_dat$link
  to_links <- to_dat$link

  to_dat[to_links %in% from_links, ]
}

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

  subset <- linked_codings[, c("link", to_columns, from_columns)]

  # If wave label is NA, that value was not included in that wave. Drop.
  subset <- subset[!is.na(subset[[paste0("label_", from)]]), ]

  from_value <- paste0("value_", from)
  to_value <- paste0("value_", to_suffix)

  recode_function(subset[[from_value]], subset[[to_value]], ...)
}
