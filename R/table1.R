#' Table 1: Stratified Descriptive Tables
#'
#' @import gtsummary gt dplyr
#'
#' @description \code{table1} wraps \code{\link[gtsummary]{tbl_summary}} with some custom options:
#'   * Condensed formatting for better space use.
#'   * Stratification in columns with informative spanning headers,
#'     plus an "overall" column if desired.
#'   * First row with total count.
#'   * Rounding of continuous values to one decimal digit. Percentages for
#'     categorical and binary are rounded to no decimal digits.
#'
#' @param ... Optional: Rows to include in table. If none are provided,
#'   all variables will be shown. Supports tidy evaluation; see examples.
#'   If multiple data sets are shown side-by-side, this parameter will be
#'   ignored; instead, subset data sets first before passing to this function.
#' @param by Optional: Stratification variable for columns. If multiple data
#'   sets are given to \code{data} as a \code{list}, then this can be a vector
#'   of corresponding variable names.
#' @param overall Optional: Show "Overall" column if stratifying? Defaults to
#'   \code{TRUE}.
#' @param label Optional: Label for strata. If empty, the variable label or
#'   variable name of the \code{by} variable will be used.
#' @param digits Optional: Level of precision (decimal digits) for statistics.
#'   By default, all continuous variables are shown with one decimal digits
#'   for both statistics, and all categorical variables are shown with integer-
#'   rounded column percentages:  \code{all_continuous() ~ c(1, 1),
#'   all_categorical()   ~ c(0, 0)}.
#'   Precision can also be changed for individual variables, e.g.:
#'   \code{all_continuous()    ~ c(1, 1),  # median, IQR limits
#'         all_categorical()   ~ c(0, 0),  # count, percent
#'         a_specific_variable ~ c(2, 2)}  # first, second statistic
#' @param statistic Optional: Summary statistic to the shown for
#'   specific variables, e.g.:
#'   \code{list(a_specific_variable ~ "{min}, {max}")}.
#'   Passed on to \code{\link[gtsummary]{tbl_summary}(statistic = ...)}.
#' @param type Optional: List with types per variable, which, at least by
#'  default, will trigger which \code{statistic} is shown. For example:
#'  \code{type = list(gear ~ "continuous", vs ~ "dichotomous")}.
#'  Passed on to \code{\link[gtsummary]{tbl_summary}(type = ...)}.
#' @param remove_border Optional. For indented lines of individual strata,
#'   remove the upper horizontal border line? Defaults to \code{TRUE}.
#' @return Formatted table. Continuous variables are
#'   median (quartile 1, quartile 3); categorical variables are
#'   count (column percent).
#' @export
#'
#' @examples
#' data(mtcars)
#'
#' # Example 1: Stratify by "gear":
#' \dontrun{
#' mtcars %>%
#'   table1(by = gear)
#' }
#'
#' # Example 2: Show only selected variables in rows:
#' \dontrun{
#' mtcars %>%
#'   table1(cyl, mpg, disp, by = gear)
#' }
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{table1a.png}{options: width=70\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{table1b.png}{options: width=70\%}}
table1 <- function(data,
                   ...,
                   by        = NULL,
                   overall   = TRUE,
                   label     = "",
                   digits    = list(all_continuous() ~ c(1, 1),
                                    all_categorical() ~ c(0, 0)),
                   statistic = NULL,
                   type      = NULL,
                   remove_border = TRUE,
    mystats <- list(N ~ "{n}")
    if(!is.null(statistic))
      mystats <- append(mystats, statistic)
    data <- data %>% dplyr::rename(.by := {{ by }})

    newdata <- data %>% select(!!!rlang::enquos(...), .data$.by)
    if(ncol(newdata) == 1)
      newdata <- data
    data <- newdata

    # Unstratified
    if(missing(by)) {
      res <- data %>%
        dplyr::mutate(N = 1) %>%
        dplyr::select(.data$N, dplyr::everything()) %>%
        gtsummary::tbl_summary(statistic = mystats, digits = digits,
                               type = type) %>%
        gtsummary::bold_labels()
      if(as_gt == TRUE) {
        res <- res %>%
          gtsummary::as_gt(include = -tab_footnote)
      }
    # Stratified
    } else {
      if(overall == TRUE) {
        if(sum(is.na(data %>%
                     dplyr::pull(.data$.by))) > 0)
          message(paste("Dropping observations where 'by' is missing (NA);",
                        "otherwise gtsummary(overall = TRUE) breaks"))
        res <- data %>%
          dplyr::mutate(N = 1) %>%
          dplyr::select(.data$N, dplyr::everything()) %>%
          dplyr::filter(!is.na(.data$.by)) %>%
          labelled::copy_labels_from(from = data) %>%
          gtsummary::tbl_summary(by = .data$.by,
                                 statistic = mystats,
                                 digits = digits,
                                 type = type) %>%
          gtsummary::add_overall() %>%
          gtsummary::modify_header(update = all_stat_cols(stat_0 = FALSE) ~
                                     "**{level}**") %>%
          gtsummary::modify_header(update = stat_0 ~ "**Overall**") %>%
          gtsummary::bold_labels()
      } else {
        res <- data %>%
          dplyr::mutate(N = 1) %>%
          dplyr::select(.data$N, dplyr::everything()) %>%
          gtsummary::tbl_summary(by = .data$.by,
                                 statistic = mystats,
                                 digits = digits,
                                 type = type) %>%
          gtsummary::modify_header(update = all_stat_cols(stat_0 = FALSE) ~
                                     "**{level}**") %>%
          gtsummary::bold_labels()
      }
      if(label == "")
        label <- paste("By", labelled::var_label(data %>%
                                                   dplyr::pull(.data$.by)))
      if(label == "By ")  # If variable label is empty, use variable name
        label <- paste("By", deparse(substitute(by)))
      if(as_gt == TRUE) {
        res <- res %>%
          gtsummary::as_gt(include = -tab_footnote) %>%
          gt::tab_spanner(label = gt::md(paste0("**", label, "**")),
                          columns = gt::matches("stat_[123456789]"))
      }
    }
  }

  # Formatting
  if(as_gt == TRUE) {
    res <- res %>%
      mytabstyle()
    rows_levels <- which(res[["_data"]]$row_type %in% c("level", "missing"))
    if(remove_border == TRUE & length(rows_levels) > 0) {
      res <- res %>%
        gt::tab_style(
          style = gt::cell_borders(sides = "top", weight = NULL),
          locations = gt::cells_body(columns = gt::everything(),
                                     rows = rows_levels))
    }
  }
  res
}
