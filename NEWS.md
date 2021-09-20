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
