# khsmisc 0.2.1

* Add alpha version of `table2()`.


# khsmisc 0.2.0

* `tabulate_rowcol()` is the replacement for `mytab()` (which remains available); it supports tidy evaluation.
* All other functions allow variable name arguments with tidy evaluation without introducing no breaking changes. `corrmat()`, `tsummary()`, and `table1()` still use all variables in a dataset if none are selected but can now take variable names as an argument.
* Breaking change: `corrmat()` argument `missing` renamed to `use` to be consistent with the underlying `stats::cor()`.
* `rates()` has new parameters `risk_time` to specify the time point at which cumulative incidence should be calculated, and `by` for stratification variables. If a dataset has already been grouped using `dplyr::group_by()`, grouping will be retained.

# khsmisc 0.1.0

* First release for the khsmisc package.
