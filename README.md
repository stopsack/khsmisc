
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `khsmisc` package

<!-- badges: start -->

<!-- badges: end -->

The khsmisc package serves two purposes: (a) loading a set of core
packages for epidemiology research and (b) providing a set of custom
functions, which also build on the core packages.

## Installation

The khsmisc package can be installed from
[GitHub](https://github.com/stopsack/khsmisc). A CRAN submission is not
planned.

``` r
# if "remotes" library is missing, install it first:
#   install.packages("remotes")
library(remotes)
remotes::install_github("stopsack/khsmisc")
```

## Loaded Core Packages

When loading `library(khsmisc)`, by design, the following dependencies
will be made available:

  - **Data handling:** the whole universe of the `tidyverse`; variable
    labels via `labelled`; reading Excel files via `readxl`.
  - **Tables:** formatted tables via `gt`; descriptive tables via
    `gtsummary`.
  - **Figures:** `ggplot2`; red/green blindness-proof color palettes via
    `viridis`; arranging figure panels via `cowplot`.
  - **Analyses:** time-to-event analyses using `survival`; accessing
    regression model results via `broom`.

## Additional Functions for Epidemiology Research

  - **Data handling:** `varlist()` for variable list/dataset inventory,
    Stata style; `write_csv_safely()`, `save_safely()`, and
    `pdf_safely()` to export CSV files, R objects, or PDFs without
    overwriting existing files.
  - **Functions for vectors:** `scale_to_range()` to scale a vector to a
    given range; `Mode()` to obtain the most common value; `roundp()` to
    round and format *p*-values; `scoreci()` for Wilson confidence
    intervals for proportions.
  - **Functions for descriptive tables:** `tabulate_rowcol()` for
    categorical variables; `tsummary()` for continuous variables;
    `mygt()` for formatting and printing any tibble as a ‘gt’ table;
    `table1()` for formatted stratified descriptive tables using
    `gtsummary`; `table2()` for stratified result tables; `rates()` for
    event counts, person-time, incidence rates, and cumulative
    incidence.
  - **Functions for figures:** `stripplot()` for box/whiskers plots
    overlaid with a dot plot of all data points; `corrmat()` for
    correlation matrices; `exclusion_flowchart()` for a flowchart with
    consequences of exclusion criteria.

# An analysis example: TCGA `BLCA`

  - The [“Get Started”](articles/khsmisc.html) vignette described an
    sample analysis workflow using the khsmisc package and TCGA `BLCA`.