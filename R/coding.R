code <- function(label, 
                 value,
                 description = label,
                 links_from = label,
                 ...) {
    stopifnot(is.character(label))
    stopifnot(is.character(description))
    stopifnot(is.character(links_from))

    check_code_entry_length(value, "codes")
    check_code_entry_length(label, "labels")
    check_code_entry_length(description, "descriptions")

    if (all(is.na(label))) {
        stop("A code cannot have a missing label.", call. = FALSE)
    }

    structure(
        list(
            label = label,
            value = value,
            description = description,
            links_from = links_from,
            ...
        ),
        class = "code"
    )
}

check_code_entry_length <- function(x, type) {
    if (length(x) > 1) {
        abort(glue(
            "Cannot have multivalued {type}.\n",
            "code() is specifically for the meaning of each value ",
            "in a variable.",
        ))
    }

    invisible(x)
}

coding <- function(...) {
    if (missing(..1)) {
        abort("At least one code must be provided.")
    }

    codes <- list(...)

    if (!all(vlapply(codes, function(x) inherits(x, "code")))) {
        abort("coding() only accepts code objects as arguments.")
    }

    labels <- vcapply(codes, function(x) x$label)

    if (any(duplicated(labels))) {
        abort("Multiple labels set in a single coding. Each label must be unique.")
    }

    values <- lapply(codes, function(x) x$value)
    values_no_na <- values[!is.na(values)]

    if (!all(vcapply(values_no_na, typeof) == typeof(values_no_na[[1]]))) {
        abort(glue("Coding types must be constant.\n",
            "Perhaps you forgot to wrap your numbers in quotes?"
        ))
    }

    structure(codes,
        class = "coding"
    )
}

is.coding <- function(x) inherits(x, "coding")

as.data.frame.coding <- function(coding, suffix = NULL) {
    # Expand links_from references to make joins easy
    coding <- lapply(coding, function(x) {
        x$label <- rep(x$label, length(x$links_from))
        x$value <- rep(x$value, length(x$links_from))
        x$description <- rep(x$description, length(x$links_from))

        x
    })

    links <- unlist(lapply(coding, function(x) x$links_from))
    labels <- unlist(lapply(coding, function(x) x$label))
    values <- unlist(lapply(coding, function(x) x$value))
    descriptions <- unlist(lapply(coding, function(x) x$description))


    out <- data.frame(
        link = links,
        label = labels,
        value = values,
        description = descriptions, 
        stringsAsFactors = FALSE
    )

    if (!is.null(suffix)) {
        stopifnot(is.character(suffix) || is_positive_integer(suffix))
        names(out) <- ifelse(names(out) == "link", names(out), paste0(names(out), "_", suffix))
    }

    out
}

print.coding <- function(coding) {
    cat("<Coding Dataframe>\n")

    if (requireNamespace("tibble", quietly = TRUE)) {
        print(tibble::as_tibble(as.data.frame(coding)))
    } else {
        print(as.data.frame(coding))
    }

    invisible()
}
