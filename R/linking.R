#' @export
link_codings <- function(to, from, to_suffix = "to", drop_unused = TRUE) {
    rc_assert(is.coding(to))

    if (!is.coding(from)) {
        if (!is.list(from)) {
            rc_err("`from` must be a coding or list of codings.")
        }

        if (!all(vlapply(from, is.coding))) {
            rc_err("Not all of `from` is a coding object.")
        }
    }

    from_dat <- if (!is.coding(from)) {
        coding_list_to_df(from)
    } else {
        as.data.frame(from, 1)
    }

    to_dat <- as.data.frame(to, to_suffix)

    if (nrow(to_dat) < nrow(from_dat)) {
        rc_err("Not all cases covered while linking codings.")
    }

    if (isTRUE(drop_unused)) {
        to_dat <- drop_unused_links(to_dat, from_dat)
    }

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

    mapped <- Map(as.data.frame, coding_list, suffixes)

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

#' @export
make_recode_query <- function(linked_codings, wave, to_suffix = "to", ...) {
    stopifnot(inherits(linked_codings, "linked_coding_df"))

    suffixed_columns <- c("label", "value")
    wave_column_patterns <- paste0(
        paste0(suffixed_columns, "_", wave), 
        collapse = "|"
    )

    to_column_patterns <- paste0(
        paste0(suffixed_columns, "_", to_suffix), 
        collapse = "|"
    )

    wave_columns <- grep(wave_column_patterns, names(linked_codings), value = TRUE)
    to_columns <- grep(to_column_patterns, names(linked_codings), value = TRUE)

    if (length(wave_columns) < 1) {
        rc_err("Wave '{wave}' not found in linked coding data.frame.")
    }

    if (length(to_columns) < 1) {
        rc_err(c(
            "'to' columns for linked coding data.frame not found.\n",
            "Perhaps '{to_suffix}' is not the 'to' column ",
            "tag you chose?"
        ))
    }

    subset <- linked_codings[, c("link", to_columns, wave_columns)]
    
    # If wave label is NA, that value was not included in that wave. Drop.
    subset <- subset[!is.na(subset[[paste0("label_", wave)]]), ]

    wave_value <- paste0("value_", wave)
    to_value <- paste0("value_", to_suffix)

    recode_function(subset[[wave_value]], subset[[to_value]], ...)
}
