# khsmisc 0.2.3

* `table1()`, `table2()`, `tsummary()`: improve input error handling
* `table2()`: allow grouping multiple strata of the effect modifier; add `type = "outcomes (risk)"`


# khsmisc 0.2.2

* `table2()`: include estimates from risk ratio and risk difference models; input checks; allow for missing effect modifier, confounders; more descriptive statistics; more examples.
* `scoreci()`: new function for Wilson score confidence intervals for proportions.


# khsmisc 0.2.1

* `table2()`: add alpha version.


# khsmisc 0.2.0

* `tabulate_rowcol()`: replacement for `mytab()` (which remains available); it supports tidy evaluation.
* All other functions allow variable name arguments with tidy evaluation without introducing no breaking changes. `corrmat()`, `tsummary()`, and `table1()` still use all variables in a dataset if none are selected but can now take variable names as an argument.
* Breaking change: `corrmat()` argument `missing` renamed to `use` to be consistent with the underlying `stats::cor()`.
* `rates()`: new parameters `risk_time` to specify the time point at which cumulative incidence should be calculated, and `by` for stratification variables. If a dataset has already been grouped using `dplyr::group_by()`, grouping will be retained.


# khsmisc 0.1.0

* First release for the khsmisc package.
