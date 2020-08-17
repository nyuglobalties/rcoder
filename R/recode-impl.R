recode_function <- function(wave_vec,
                            to_vec,
                            ...,
                            .engine = names(default_engines),
                            .engines = default_engines) {
  rc_assert(length(wave_vec) == length(to_vec))
  rc_assert(length(wave_vec) > 0)

  .engine <- match.arg(.engine)

  if (!is.function(.engines[[.engine]])) {
    rc_err("'{engine}' found, but engine is not a function.")
  }

  .engines[[.engine]](wave_vec, to_vec, ...)
}

recode_function_dplyr <- function(from, to) {
  f <- function(.x, .y) bquote(x == .(.x) ~ .(.y))

  exprs <- unname(Map(f, from, to))
  body <- rlang::call2("case_when", !!!exprs, .ns = "dplyr")

  new_recode_func(body)
}

recode_function_tidyfast <- function(from, to) {
  f <- function(.x, .y) bquote(x == .(.x) ~ .(.y))

  exprs <- unname(Map(f, from, to))
  body <- rlang::call2("dt_case_when", !!!exprs, .ns = "tidyfast")

  new_recode_func(body)
}

new_recode_func <- function(body) {
  rlang::new_function(alist(x=), body)
}
