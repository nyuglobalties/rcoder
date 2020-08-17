
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcoder

> rcoder outlines a lightweight data structure that captures categorical
> codings and easily converts them to other implementations.

<!-- badges: start -->

[![R build
status](https://github.com/nyuglobalties/rcoder/workflows/R-CMD-check/badge.svg)](https://github.com/nyuglobalties/rcoder/actions)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Usage

rcoder’s main functions are `coding()` and `code()`. `code()` maps a
character-valued key to some value to represent a categorical level.

``` r
library(rcoder)
library(magrittr)

code("Yes", 1)
#> <Code>
#> label: 'Yes'
#> value: 1
```

`code()` objects can hold an arbitrary amount of metadata. Useful pieces
of information to know are descriptions of what levels represent (in
case the label doesn’t have enough information) and whether or not the
value represents a missing value.

``` r
code("No response", -99, description = "Surveyed individual ignored question when asked", missing = TRUE)
#> <Code>
#> label: 'No response'
#> value: -99
#> description: 'Surveyed individual ignored question when asked'
#> Represents a missing value
```

`coding()` is a collection of `code()` objects that represents the full
set of values for a categorical variable.

``` r
coding(
  code("Don't know", 0, missing = TRUE),
  code("Never", 1),
  code("Rarely", 2),
  code("Sometimes", 3),
  code("Frequently", 4),
  code("Always", 5),
  code("No response", -99, missing = TRUE),
  code("Refused", -88, missing = TRUE),
  code("Absent", -77, missing = TRUE)
)
#> <Coding>
#> # A tibble: 9 x 5
#>   link        label       value description missing
#>   <chr>       <chr>       <dbl> <chr>       <lgl>  
#> 1 Don't know  Don't know      0 Don't know  TRUE   
#> 2 Never       Never           1 Never       FALSE  
#> 3 Rarely      Rarely          2 Rarely      FALSE  
#> 4 Sometimes   Sometimes       3 Sometimes   FALSE  
#> 5 Frequently  Frequently      4 Frequently  FALSE  
#> 6 Always      Always          5 Always      FALSE  
#> 7 No response No response   -99 No response TRUE   
#> 8 Refused     Refused       -88 Refused     TRUE   
#> 9 Absent      Absent        -77 Absent      TRUE
```

`coding()` objects are designed to be an intermediate representation of
categorical data so that they can be converted into different
representations on the fly, e.g. ODK XLSForm choices and STATA/SPSS
columns via [`haven`](https://haven.tidyverse.org/).

``` r
coding(
  code("Never", 1),
  code("Rarely", 2),
  code("Sometimes", 3),
  code("Frequently", 4),
  code("Always", 5),
  .label = "frequency"
) %>% 
  coding_to_odk()
#> # A tibble: 5 x 3
#>   list_name  name label     
#>   <chr>     <dbl> <chr>     
#> 1 frequency     1 Never     
#> 2 frequency     2 Rarely    
#> 3 frequency     3 Sometimes 
#> 4 frequency     4 Frequently
#> 5 frequency     5 Always
```

To facilitate recoding, `coding()` objects link to one another through
the `code()` labels. If multiple values are collapsed into one, use the
`links_from` parameter to identify which values are combined into one.

``` r
original_coding <- coding(
  code("No", 0L),
  code("Yes", 1L),
  code("No response", -99L, missing = TRUE),
  code("Refused", -88L, missing = TRUE),
  code("Absent", -77L, missing = TRUE)
)

new_coding <- coding(
  code("No", 0L),
  code("Yes", 1L),
  code("Missing", NA, links_from = c("No response", "Refused", "Absent"))
)

new_coding %>% 
  link_codings(original_coding)
#>          link label_to value_to     label_1 value_1
#> 1      Absent  Missing       NA      Absent     -77
#> 2          No       No        0          No       0
#> 3 No response  Missing       NA No response     -99
#> 4     Refused  Missing       NA     Refused     -88
#> 5         Yes      Yes        1         Yes       1
```

These linked codings can be converted into a function that accepts a
vector and returns a recoded vector.

``` r
new_coding %>% 
  link_codings(original_coding) %>% 
  make_recode_query()
#> function (x) 
#> dplyr::case_when(x == -77L ~ NA_integer_, x == 0L ~ 0L, x == 
#>     -99L ~ NA_integer_, x == -88L ~ NA_integer_, x == 1L ~ 1L)
#> <environment: 0x7fa9e6633478>
```

## Installation

rcoder is not yet on CRAN, so you will need to install from this
repository:

``` r
install.packages("remotes")
remotes::install_github("nyuglobalties/rcoder")
```
