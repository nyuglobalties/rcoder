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

recode_function_dplyr <- function(from, to, guard = FALSE) {
  f <- function(.x, .y) bquote(x == .(.x) ~ .(.y))

  exprs <- unname(Map(f, from, to))
  body <- rlang::call2("case_when", !!!exprs, .ns = "dplyr")

  new_recode_func(body, from, from_guard = guard)
}

recode_function_tidyfast <- function(from, to, guard = FALSE) {
  f <- function(.x, .y) bquote(x == .(.x) ~ .(.y))

  exprs <- unname(Map(f, from, to))
  body <- rlang::call2("dt_case_when", !!!exprs, .ns = "tidyfast")

  new_recode_func(body, from, from_guard = guard)
}

new_recode_func <- function(body, from, from_guard) {
  # Keep recoding attribute-safe
  body <- bquote({
    attrs <- attributes(x)
    x <- .(body)
    attributes(x) <- attrs
    x
  })

  if (!identical(from_guard, FALSE)) {
    full_body <- bquote({
      all_from <- .(c(unique(from), NA))
      guard_type <- .(from_guard)

      if (isTRUE(guard_type)) {
        guard_type <- "hard"
      }

      if (!all(unique(x) %in% all_from)) {
        missing_cases <- setdiff(x, all_from)

        if (is.character(missing_cases)) {
          missing_cases <- paste0("'", missing_cases, "'")
        }

        cond_msg <- paste0(
          "Not all cases are covered: [",
          paste0(missing_cases, collapse = ","),
          "]"
        )

        if (identical(guard_type, "hard")) {
          stop(
            cond_msg,
            call. = FALSE
          )
        } else {
          message(cond_msg)
        }
      }

      .(body)
    })
  } else {
    full_body <- body
  }

  rlang::new_function(alist(x = ), full_body)
}
