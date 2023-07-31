#' khsmisc: Miscellaneous functions for epidemiology research
#'
#' This package serves two purposes: (a) loading a set of core packages
#' for epidemiology research and (b) providing a set of custom functions,
#' which also build on the core packages.
#'
#' @section Automatically loaded core packages:
#'
#' This package loads a number of useful core packages for epidemiologic
#' data handling as well as descriptive and inferential analyses.
#'
#'   * Data handling: the whole universe of the \code{\link[tidyverse]{tidyverse-package}};
#'     variable labels via \code{\link[labelled]{labelled}};
#'     reading Excel files via \code{\link[readxl]{readxl-package}}.
#'   * Tables: formatted tables via \code{\link[gt]{gt}};
#'     descriptive tables via \code{\link[gtsummary]{gtsummary-package}}.
#'   * Figures: \code{\link[ggplot2]{ggplot}};
#'     red/green blindness-proof color palettes via
#'     \code{\link[viridis]{scale_color_viridis}};
#'     arranging figure panels via cowplot (\code{\link[cowplot]{plot_grid}}).
#'   * Analyses: time-to-event analyses using \code{survival};
#'     accessing regression model results via \code{\link[broom]{broom}}.
#'
#' @section Functions for data handling:
#'
#' \code{\link[khsmisc]{varlist}}: Variable list/dataset inventory, Stata style
#'
#' \code{\link[khsmisc]{write_csv_safely}}, \code{\link[khsmisc]{save_safely}},
#'  \code{\link[khsmisc]{pdf_safely}}:
#'  Export CSV files, R objects, or PDFs without overwriting existing files
#'
#' @section Functions for vectors:
#'
#' \code{\link[khsmisc]{scale_to_range}}: Scale a vector to a given range
#'
#' \code{\link[khsmisc]{Mode}}: Most common value
#'
#' \code{\link[khsmisc]{roundp}}: Round and format p-values
#'
#' @section Functions for descriptive tables:
#'
#' \code{\link[khsmisc]{tabulate_rowcol}}: A contingency table for categorical
#'   variables.
#'
#' \code{\link[khsmisc]{tsummary}}: A descriptive table for continuous variables
#'
#'
#' \code{\link[khsmisc]{rates}}: Event counts, person-time, incidence rates, and cumulative incidence
#'
#' @section Functions for plots:
#'
#' \code{\link[khsmisc]{stripplot}}: Box-whiskers/dot plot
#'
#' \code{\link[khsmisc]{corrmat}}: Correlation matrix
#'
#' \code{\link[khsmisc]{exclusion_flowchart}}: Flowcharts of exclusions from a study
#'
#' \code{\link[khsmisc]{geom_stepribbon}}: Step ribbon plots
#'
#' \code{\link[khsmisc]{brickchart}}: Brick chart
#'
#' @docType package
#' @name khsmisc
#' @keywords internal
#' @seealso \url{https://github.com/stopsack/khsmisc}
NULL
#> NULL
