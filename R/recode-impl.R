recode_function <- function(wave_vec, 
                            to_vec, 
                            ..., 
                            .engine = names(default_engines), 
                            .engines = default_engines) {
    stopifnot(length(wave_vec) == length(to_vec), length(wave_vec) > 0)

    .engine <- match.arg(.engine)

    if (!is.function(.engines[[.engine]])) {
        abort(glue("'{engine}' found, but engine is not a function."))
    }

    .engines[[.engine]](wave_vec, to_vec, ...)
}

recode_function_dplyr <- function(from, to) {
    f <- function(.x, .y) bquote(x == .(.x) ~ .(.y))

    exprs <- unname(Map(f, from, to))
    body <- rlang::call2("case_when", !!!exprs, .ns = "dplyr")

    eval(bquote(function(x) .(body)))
}

recode_function_tidyfast <- function(from, to) {
    f <- function(.x, .y) bquote(x == .(.x) ~ .(.y))

    exprs <- unname(Map(f, from, to))
    body <- rlang::call2("dt_case_when", !!!exprs, .ns = "tidyfast")

    eval(bquote(function(x) .(body)))
}
