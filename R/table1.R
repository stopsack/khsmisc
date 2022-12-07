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
#' @param data Data frame/tibble to print. Required. Alternatively, this can
#'   be a \code{list} of data frames, which will be displayed side-by-side.
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
#' @param as_gt Optional. Whether to return a formatted gt table, or, if
#'   knitting to a \code{github_document}, a kable table. Defaults to
#'   \code{TRUE}.
#' @param data_names Optional. A character vector of names when displaying
#'   multiple data sets (i.e., when inputting a \code{list} to the \code{data}
#'   argument)
#'
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
#'
#' # Example 3: Two datasets side by side
#' \dontrun{
#' dataset1 <- mtcars %>%
#'   dplyr::select(cyl, hp, qsec, gear) %>%
#'   dplyr::slice(1:20)
#'
#' dataset2 <- mtcars %>%
#'   dplyr::select(cyl, hp, wt, gear) %>%
#'   dplyr::slice(5:20)
#'
#' table1(
#'   data = list(dataset1, dataset2),
#'   by = gear,
#'   data_names = c("First set", "Second set"))
#' }
#'
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
                   as_gt     = TRUE,
                   data_names = NULL) {
  # Multiple data sets side-by-side
  if("list" %in% class(data)) {
    if(missing(by)) {
      tbl_list <- purrr::map(
        .x = data,
        .f = ~table1(data = .x,
                     overall = overall,
                     label = label,
                     digits = digits,
                     statistic = statistic,
                     type = type,
                     as_gt = FALSE))
    } else {
      if(length(as.list(match.call(expand.dots=FALSE))$by) == 1) {
        bylist <- rep(as.character(as.list(match.call(expand.dots = FALSE))$by),
                      times = length(data))
      } else {
        bylist <- as.character(as.list(match.call(expand.dots = FALSE))$by)[-1]
      }
      tbl_list <- purrr::map2(
        .x = data,
        .y = bylist,
        .f = ~table1(data = .x %>%
                       dplyr::rename(".by" = one_of(.y)),
                     by = .data$.by,
                     overall = overall,
                     label = label,
                     digits = digits,
                     statistic = statistic,
                     type = type,
                     as_gt = FALSE))
    }
    res <- gtsummary::tbl_merge(tbls = tbl_list,
                                tab_spanner = data_names)
    if(any(stringr::str_detect(
      string = c("", knitr::opts_knit$get("rmarkdown.pandoc.to")),
      pattern = "gfm"))) {
      res <- res %>%
        gtsummary::as_kable()
    } else {
      res <- res %>%
        gtsummary::as_gt(include = -tab_footnote)
    }

  # Single data set
  } else {
    gtsummary::set_gtsummary_theme(
      list("tbl_summary-fn:percent_fun" = function(x)
        sprintf("%.0f", 100 * x)))
    mystats <- list(N ~ "{n}")
    if(!is.null(statistic))
      mystats <- append(mystats, statistic)

    # Unstratified
    if(missing(by)) {
      newdata <- data %>%
        dplyr::select(!!!rlang::enquos(...))
      if(ncol(newdata) == 0)
        newdata <- data
      data <- newdata
      res <- data %>%
        dplyr::mutate(N = 1) %>%
        dplyr::select(.data$N, dplyr::everything()) %>%
        gtsummary::tbl_summary(statistic = mystats, digits = digits,
                               type = type) %>%
        gtsummary::bold_labels()
      if(as_gt == TRUE) {
        if(any(stringr::str_detect(
          string = c("", knitr::opts_knit$get("rmarkdown.pandoc.to")),
          pattern = "gfm"))) {
          res <- res %>%
            gtsummary::as_kable()
        } else {
          res <- res %>%
            gtsummary::as_gt(include = -tab_footnote)
        }
      }
    # Stratified
    } else {
      data <- data %>%
        dplyr::rename(".by" := {{ by }})
      newdata <- data %>%
        dplyr::select(
          !!!rlang::enquos(...),
          ".by")
      if(ncol(newdata) == 1)
        newdata <- data
      data <- newdata

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
        if(any(stringr::str_detect(
          string = c("", knitr::opts_knit$get("rmarkdown.pandoc.to")),
          pattern = "gfm"))) {
          res <- res %>%
            gtsummary::as_kable()
        } else {
          res <- res %>%
            gtsummary::as_gt(include = -tab_footnote) %>%
            gt::tab_spanner(label = gt::md(paste0("**", label, "**")),
                            columns = gt::matches("stat_[123456789]"))
        }
      }
    }
  }

  # Formatting
  if(as_gt == TRUE &
     !any(stringr::str_detect(
       string = c("", knitr::opts_knit$get("rmarkdown.pandoc.to")),
       pattern = "gfm"))) {
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
