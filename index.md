
# The `khsmisc` package

<!-- badges: start -->
[![R CMD Check Windows/MacOS/Ubuntu](https://github.com/stopsack/khsmisc/actions/workflows/r.yml/badge.svg)](https://github.com/stopsack/khsmisc/actions/workflows/r.yml)
<!-- badges: end -->

The khsmisc package provides a set of custom functions for epidemiology
research. For a package that loads core libraries that fit well with
this approach (e.g., tidyverse), see the [khsverse
package](https://stopsack.github.io/khsverse).

## Installation

The khsmisc package can be installed or updated from
[GitHub](https://github.com/stopsack/khsmisc). A CRAN submission is not
planned. If the remotes library is missing, install it first 
(`install.packages("remotes")`).

Install or update the khsmisc package:
``` r
remotes::install_github("stopsack/khsmisc")
```

## Functions for Epidemiology Research

-   **Data handling:** `varlist()` for variable list/dataset inventory,
    Stata style; `write_csv_safely()`, `save_safely()`,
    `saveRDS_safely()`, and `pdf_safely()` to export CSV files, R
    objects, or PDFs without overwriting existing files; and `make_exclusions()` 
    to perform sequential exclusions when generating an analytical dataset.
-   **Functions for vectors:** `scale_to_range()` to scale a vector to a
    given range; `Mode()` to obtain the most common value; and `roundp()` to
    round and format *p*-values. 
-   **Estimation functions:** `estimate_rmtl()` for restricted mean time lost
    and its difference.
-   **Functions for descriptive tables:** `tabulate_rowcol()` for
    categorical variables; `tsummary()` for continuous variables. 
-   **Functions for figures:** `stripplot()` for box/whiskers plots
    overlaid with a dot plot of all data points; `corrmat()` for
    correlation matrices; `exclusion_flowchart()` for a flowchart with
    consequences of exclusion criteria; `brickchart()` for plotting
    proportions while showing individual observations.
-   **Others:** R Markdown template, available via File &gt; New
    File &gt; R Markdown &gt; From Template.
    
**Note** The functions 

* `table1()` for formatted descriptive tables via gtsummary,
* `table2()` for stratified result tables,
* `mygt()` for formatting/printing tables as a ‘gt’ object, and
* estimation functions `rates()`, `scoreci()`, and `survdiff_ci()`

have been merged into functionality now available in the [rifttable package](https://stopsack.github.io/rifttable) and are being maintained there.


## An analysis example: TCGA `BLCA`

-   The [“Get Started”](articles/khsmisc.html) vignette described an
    sample analysis workflow using the khsmisc package and TCGA `BLCA`.
