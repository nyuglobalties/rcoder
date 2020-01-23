#' @export
code <- function(label, 
                 value,
                 description = label,
                 links_from = label,
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
            ...
        ),
        class = "code"
    )
}

print.code <- function(x, ...) {
    cat("<Code>\n")
    print(glue("label: {ui_value(x$label)}"))
    print(glue("value: {x$value}"))

    if (!identical(x$description, x$label)) {
        print(glue("description: {ui_value(x$description)}"))
    }

    if (!identical(x$links_from, x$label)) {
        print(glue("linking from: [{glue_collapse(ui_value(x$links_from), ', ')}]"))
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
coding <- function(...) {
    if (missing(..1)) {
        return(empty_coding())
    }

    codes <- list(...)

    if (!all(vlapply(codes, function(x) inherits(x, "code")))) {
        rc_err("coding() only accepts code objects as arguments.")
    }

    labels <- vcapply(codes, function(x) x$label)

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

    structure(codes,
        class = "coding"
    )
}

#' @export
empty_coding <- function() {
    structure(list(),
        class = "coding"
    )
}

#' @export
is_empty_coding <- function(x) {
    identical(x, empty_coding())
}

is.coding <- function(x) inherits(x, "coding")

coding_values <- function(coding) {
    stopifnot(is.coding(coding))

    if (is_empty_coding(coding)) {
        return(logical())
    }

    unlist(lapply(coding, function(code) code$value))
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

as.data.frame.coding <- function(coding, suffix = NULL) {
    contents <- coding_contents(coding)

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

print.coding <- function(coding) {
    cat("<Coding>\n")

    if (requireNamespace("tibble", quietly = TRUE)) {
        print(tibble::as_tibble(as.data.frame(coding)))
    } else {
        print(as.data.frame(coding))
    }

    invisible()
}
