
# The `khsmisc` package

<!-- badges: start -->
[![R CMD Check Windows/MacOS/Ubuntu](https://github.com/stopsack/khsmisc/actions/workflows/r.yml/badge.svg)](https://github.com/stopsack/khsmisc/actions/workflows/r.yml)
<!-- badges: end -->

The khsmisc package provides a set of custom functions for epidemiology
research. For a package that loads core libraries that fit well with
this approach (e.g., tidyverse), see the [khsverse
package](https://stopsack.github.io/khsverse).

## Installation

The khsmisc package can be installed from
[GitHub](https://github.com/stopsack/khsmisc). A CRAN submission is not
planned.

``` r
# if "remotes" library is missing, install it first:
#   install.packages("remotes")
remotes::install_github("stopsack/khsmisc")
```

## Functions for Epidemiology Research

-   **Data handling:** `varlist()` for variable list/dataset inventory,
    Stata style; `write_csv_safely()`, `save_safely()`,
    `saveRDS_safely()`, and `pdf_safely()` to export CSV files, R
    objects, or PDFs without overwriting existing files.
-   **Functions for vectors:** `scale_to_range()` to scale a vector to a
    given range; `Mode()` to obtain the most common value; `roundp()` to
    round and format *p*-values; `scoreci()` for Wilson confidence
    intervals for proportions.
-   **Functions for descriptive tables:** `tabulate_rowcol()` for
    categorical variables; `tsummary()` for continuous variables;
    `mygt()` for formatting and printing any tibble as a ‘gt’ table;
    `table1()` for formatted stratified descriptive tables using
    `gtsummary`; `table2()` for stratified result tables; `rates()` for
    event counts, person-time, incidence rates, and cumulative
    incidence.
-   **Functions for figures:** `stripplot()` for box/whiskers plots
    overlaid with a dot plot of all data points; `corrmat()` for
    correlation matrices; `exclusion_flowchart()` for a flowchart with
    consequences of exclusion criteria; `geom_stepribbon()` for
    visualizing RMST differences
-   **Others:** R Markdown template, available via File &gt; New
    File &gt; R Markdown &gt; From Template.

## An analysis example: TCGA `BLCA`

-   The [“Get Started”](articles/khsmisc.html) vignette described an
    sample analysis workflow using the khsmisc package and TCGA `BLCA`.
