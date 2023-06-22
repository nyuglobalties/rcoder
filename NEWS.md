# rcoder 0.2.3
* Fixes issue where coding deparsing could result in multi-length character vector

# rcoder 0.2.2
* Fixes issue where recoding would lose attributes of recoded vector

# rcoder 0.2.1

* Adds `missing_codes()` to extract the codes that represent missing values

# rcoder 0.2.0

* `link_codings` now doesn't drop unused links by default, preferring to error out instead
* `link_codings(.drop_unused)` now signals to drop any links in `from` that aren't captured in `to`, harmonizing information flow
* `as.character.coding` now doesn't include `links_from` by default. If you'd like to include that, use `as.character(include_links_from = TRUE)`
* Missing `value` statements to `code()` now are reported in error message for better debugging experience

# rcoder 0.1.4

* Adds `to` and `from` members to incomplete linking error object for end-user diagnostics

# rcoder 0.1.3

* Allows "bpr.coding" attributes to be suitable sources for recoding if "rcoder.coding" is not defined

# rcoder 0.1.2

* Adds {blueprintr} variable decoration support during assigning coding or recoding of vectors

# rcoder 0.1.1

* Adds `recode_vec` and `assign_coding`, simple interfaces for recoding vectors and embedding codings as attributes in vectors, respectively
* Adds a check to see if codings to be linked have no common links. A diagnostic error message is now displayed to track back the issue in the user code.

# rcoder 0.1.0

* Initial release
