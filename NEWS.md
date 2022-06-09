# khsmisc 0.4.2

* `make_exclusions()`: return a list of observations filtered out (`excluded`)
* `brickchart()`: do not drop `by` or `group` categories without events
* New vignette about ratio of continuous outcomes with `table2()`


# khsmisc 0.4.1

* `table1()` and `table2() %>% mygt()`: Remove horizontal border lines at the 
     top of indented lines (parameter `remove_border`).
* `mygt()`:
   + Return raw HTML if knitting R Markdown to `output: github_document`.
   + Format column labels.
* `table2()`: 
   + Pass along indentation of labels to `mygt()` as an attribute instead of
     HTML.
   + Improve handling of missing effect modifier strata, empty exposure strata,
     uniform binary exposures, `logical` effect modifiers, 
     missing per-stratum estimates, and rounding of 
     `type = "time"` and `"events/time"`.
   + New `type = "range"` (min-max).
* `brickchart()`: make compatible (without legend) with ggplot >= 3.3.4;
  make facets customizable.
* `roundp()`: format *p* as `> 0.99` instead of `1.00`.
* Update R Markdown template.


# khsmisc 0.4.0

* `make_exclusions()`: New function for sequential exclusions to derive an 
  analytical data set and prepare a flow chart for `exclusion_flowchart()`.
* `estimate_rmtl()`: New function, reshaping code by Conner and Trinquart, that
  estimates restricted mean time lost, handling competing risks and weighting.
* `survdiff_ci()`: New function for differences in *x*-year survival/cumulative
  incidence.
* `table2()`:
   + **Breaking change**: In the `design` matrix, digits for rounding can no 
     longer be provided as part of `type`. Use separate column `digits` if
     needed.
   + Add unstratified, descriptive tables without an `exposure` variable, as
     well as showing overall and stratified estimates in one table.
   + Allow for custom estimation functions
     (`table2(custom = list(fun_a, fun_b)))` that can be called via, *e.g.*, 
     `type = "custom1"`.
   + Add more predefined `type` options: `medsurv` median survival, 
     `medfu`/`maxfu` median/maximum follow-up, `surv`/`cuminc` survival and 
     cumulative incidence  from Kaplan-Meier, `rmtl` restricted mean time lost, 
     `rmtdiff`/`survdiff`/`cumincdiff` difference in RMTL/survival/cumulative 
     incidence, `mean (sd)`/`sd` mean and standard deviation.
   + If a regression model cannot provide estimates for a stratum, print `--`.
   + Add argument `risk_digits` for rounding of risk/cumulative incidence.
   + Fix `type = "median (iqr)"`: do not remove missing values, like elsewhere.
* Add screenshots of formatted output to `man/figures/`.
* Require up-to-date gt, gtsummary dependencies.


# khsmisc 0.3.6

* `table1()`: 
   + Set rounding of column percentages for categorical (and binary) 
     variables to 0 decimal digits by default.
   + Add optional `type` parameter, passed along to `gtsummary::tbl_summary()`.
   + Update internal syntax and require gtsummary version >= 1.3.6.
* `table2()`: 
   + Add optional `type2` column to the `design` matrix to allow adding a 
     second estimate per cell.
   + Add `prepare_md` parameter and example for how to obtain a 
     markdown-formatted `label` column (e.g., `**bold**` for a **bold** label).
* `mygt()`: add `md` option to get markdown formatting on selected columns
   through `gt::fmt_markdown(columns = md)`


# khsmisc 0.3.5

* `table2()`: 
   + Add `trend` to the `design` matrix to allow for estimating linear slopes
     for a continuously-coded exposure.
   + Fix accidental handling of `type = "irr"` as an RR model with
     approach "i".


# khsmisc 0.3.4

* `brickchart()`: Auto-generate color palette for an unlimited number of
  exposure categories. Expand examples.
* `DESCRIPTION`: Add `Suggests: markdown` to fix github-actions.
* `table2()`: For risk ratios and risk differences, allow passing on model
  fitting approach and number of bootstrap repeats.


# khsmisc 0.3.3

* Change many examples after the `ovarian` dataset was removed from the survival
  package; use (lung) `cancer` instead.
  

# khsmisc 0.3.2

* `table2()`: 
   + Fix input check for `outcome` variable for `type %in% c("fold", "or")`.
   + Digits for rounding: Fix regular expression for parentheses.
   + Add `type = "cases/controls"`.
   + Add `type = "foldlog"` for models of geometric means.
   + Add `type = "quantreg"` for differences from quantile regression.
   + Line breaks.
* `brickchart()`: New function for "brick charts"--proportions by category,
   where "bricks" display individual observations.


# khsmisc 0.3.1

* `table2()`: Fix exposure category order if starting table with `"blank"` row.
* `khsmisc_rmarkdown_template`: custom RMarkdown template
* `saveRDS_safely()`: equivalent to `save_safely()` for single objects and 
  `.rds` files
* `geom_stepribbon()`: ggplot aesthetic to highlight the difference between
  two Kaplan-Meier curves, corresponding to the difference in RMST


# khsmisc 0.3.0

* Remove all packages loaded via `Depends:`. Instead, provide the khsverse 
  package as an alternative package loader if desired. Remove readxl and 
  magrittr dependencies altogether.
* `DESCRIPTION`: Add `Remotes:` to install [risks package from 
  Github](https://stopsack.github.io/risks)
* `table2()`: Add left-truncated survival models, using `time` and `time2` 
  elements in the `design` matrix.
* Require readr >= 1.4.0 and change `path =` argument of `write_csv_safely()` 
  to `file =`, as in `save_safely()` and `pdf_savely()`.


# khsmisc 0.2.6

* `table2()`: 
   + Add `type = "or"` for logistic models.
   + Fix "fold" (non-binary).
   + Fix vector issue in use of `digits_...`.
* Update vignette to make use of `table2()`.


# khsmisc 0.2.5

* `table2()`: 
   + Generalize regression model fitting framework in one function.
   + Add Poisson models and Gaussian models with log link.
   + Add median/IQR to descriptive statistics.
   + Reshape requesting digits for rounding: `diff_digits`, `ratio_digits`, 
     and `rate_digits` are global parameters of `table2()` that can be 
     overridden using `type = "(estimand) (digits)"` in each line.
   + In joint models, use the reference categories of the exposure and effect 
     modifier factors.


# khsmisc 0.2.4

* `exclusion_flowchart()`: visualize exclusions from study with a flowchart via
  `DiagrammeR::grViz()`.
* `table2()`: 
   + For vectors that are not categorical, force `exposure` to be a factor and 
     issue a warning, but do not abort.
   + Allow for blank lines.
   + Handle continuous outcomes variables with `type = "diff"` and 
     `type = "mean"`.


# khsmisc 0.2.3

* `table1()`, `table2()`, `tsummary()`: improve input error handling
* `table2()`: allow grouping multiple strata of the effect modifier; add 
  `type = "outcomes (risk)"`


# khsmisc 0.2.2

* `table2()`: include estimates from risk ratio and risk difference models; 
  input checks; allow for missing effect modifier, confounders; more 
  descriptive statistics; more examples.
* `scoreci()`: new function for Wilson score confidence intervals for 
  proportions.


# khsmisc 0.2.1

* `table2()`: add alpha version.


# khsmisc 0.2.0

* `tabulate_rowcol()`: replacement for `mytab()` (which remains available); 
  it supports tidy evaluation.
* All other functions allow variable name arguments with tidy evaluation 
  without introducing breaking changes. `corrmat()`, `tsummary()`, and 
  `table1()` still use all variables in a dataset if none are selected but can 
  now take variable names as an argument.
* Breaking change: `corrmat()` argument `missing` renamed to `use` to be 
  consistent with the underlying `stats::cor()`.
* `rates()`: new parameters `risk_time` to specify the time point at which 
  cumulative incidence should be calculated, and `by` for stratification 
  variables. If a dataset has already been grouped using `dplyr::group_by()`, 
  grouping will be retained.


# khsmisc 0.1.0

* First release for the khsmisc package.
