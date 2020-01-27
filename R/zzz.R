default_engines <- NULL

.onLoad <- function(libname, pkgname) {

  default_engines <<- list(
    dplyr = recode_function_dplyr,
    tidyfast = recode_function_tidyfast
  )

  invisible()
}
