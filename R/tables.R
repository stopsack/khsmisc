# Functions for (descriptive) tables

#' Contingency Table
#'
#' @import tidyverse purrr dplyr forcats
#' @importFrom rlang `:=`
# @import dplyr forcats tidyr purrr
#'
#' @description Tabulates X times Y. Using a data frame (tibble)
#'   as input, the first and second columns of the data will be used
#'   as rows and columns, respectively.
#'
#' @param data Data frame (tibble).
#' @param maxlevels Optional. Maximum number of levels for rows and columns.
#'   If a row or column variable has more than this number of levels,
#'   for example because it is actually a continuous variable,
#'   then only a warning, not a hard error, will be shown. Defaults to 30.
#' @param rowsums Optional. Show row sums? Defaults to TRUE.
#' @param colsums Optional. Show column sums? Defaults to TRUE.
#'
#' @return Tibble.
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars %>%
#'   select(gear, carb) %>%
#'   mytab()
#'
#' # Swap rows and columns, turn off totals:
#' mtcars %>%
#'   select(carb, gear) %>%
#'   mytab(rowsum = FALSE, colsum = FALSE)
mytab <- function(data,
                  maxlevels = 30,
                  rowsums   = TRUE,
                  colsums   = TRUE) {
  var1lvls <- length(levels(factor(data %>% dplyr::pull(1))))
  var2lvls <- length(levels(factor(data %>% dplyr::pull(2))))
  topname <- paste(colnames(data)[1], "X", colnames(data)[2])
  if(var2lvls > maxlevels)
    warning(paste("No table is printed because the column variable has", var2lvls,
                  "levels. Maximum is set to", maxlevels))
  else {
    if(var1lvls > maxlevels)
      warning(paste("No table is printed because the row variable has", var1lvls,
                    "levels. Maximum is set to", maxlevels))
    else {
      result <- data %>%
        dplyr::mutate_at(.vars = dplyr::vars(1:2), .funs = as.factor) %>%
        dplyr::mutate_at(.vars = dplyr::vars(1:2), .funs = forcats::fct_explicit_na) %>%
        dplyr::group_by_at(.vars = dplyr::vars(1, 2)) %>%
        dplyr::summarize(n = dplyr::n()) %>%
        tidyr::spread(key = 2, value = "n") %>%
        dplyr::mutate_at(.vars = dplyr::vars(-1), .funs = ~ifelse(is.na(.), 0, .)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(!!topname := 1)
      if(rowsums == TRUE) {
        result <- result %>%
          dplyr::mutate_at(.vars = dplyr::vars(1), .funs = as.character)
        result <- result %>%
          dplyr::bind_rows(dplyr::summarize_all(result, .funs = function(x) {
            if(is.numeric(x)) sum(x) else "Total" }))
      }
      if(colsums == TRUE)
        result <- result %>%
          dplyr::mutate(Total = purrr::pmap_dbl(.l = dplyr::select(result, -1), .f = sum))
      result
    }
  }
}


#' Summary of All Numeric Variables in a Data Frame
#'
#' @import tidyr dplyr
#'
#' @description Calculate descriptive summary statistics of
#'   all numeric variables in a given dataset. Optionally, this output
#'   can be stratified by one or more categorical variable(s).
#'
#' @param data Data frame (tibble).
#' @param by Optional. Categorical variable(s) to stratify results by.
#' @param na.rm Optional. Drop missing values from summary statatistics?
#'   If set to \code{FALSE}, summary statistics may be missing in the presence of
#'   missing values. Defaults to \code{TRUE}.
#'
#' @return Tibble, possibly grouped, with the following columns:
#'   * \code{rows} Row count
#'   * \code{obs} Count of non-missing observations
#'   * \code{distin} Count of distinct values
#'   * \code{min} Minimum value
#'   * \code{q25} 25th percentile
#'   * \code{median} Median, 50th percentile
#'   * \code{q75} 75th percentile
#'   * \code{max} Maximum value
#'   * \code{mean} Mean
#'   * \code{sd} Standard deviation
#'   * \code{sum} Sum of all values
#'
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars %>%
#'   select(mpg, cyl, hp, am, gear, carb) %>%
#'   tsummary()
#'
#' # Remove certain summary statistics:
#' mtcars %>%
#'   select(mpg, cyl, hp, am, gear, carb) %>%
#'   tsummary() %>%
#'   select(-mean, -sd, -sum)
#'
#' # Stratify by 'gear':
#' mtcars %>%
#'   select(mpg, hp, gear, carb) %>%
#'   tsummary(by = "gear")
#'
#' # Stratify by 'gear' and 'am':
#' mtcars %>%
#'   select(mpg, hp, gear, am, carb) %>%
#'   tsummary(by = c("am", "gear"))
tsummary <- function(data,
                     by    = NULL,
                     na.rm = TRUE) {
  data %<>% labelled::remove_labels()
  if(is.null(by)) {
    data %<>%
      dplyr::select_if(is.numeric) %>%
      tidyr::gather(key = "variable", value = "value") %>%
      dplyr::group_by(.data$variable)
  } else {
    var1 <- "variable"
    to_keep <- c(data %>% dplyr::select_if(is.numeric) %>% names(), by)
    data %<>%
      dplyr::select(dplyr::one_of(to_keep)) %>%
      tidyr::gather(key = "variable", value = "value",
                    -dplyr::contains(paste(by))) %<>%
      dplyr::group_by_at(dplyr::vars(dplyr::all_of(var1), dplyr::all_of(by)))
  }
  data %>% dplyr::summarize(rows   = dplyr::n(),
                            obs    = sum(!is.na(.data$value)),
                            distin = dplyr::n_distinct(.data$value),
                            min    = min(.data$value,                   na.rm = na.rm),
                            q25    = stats::quantile(.data$value, 0.25, na.rm = na.rm),
                            median = stats::median(.data$value,         na.rm = na.rm),
                            q75    = stats::quantile(.data$value, 0.75, na.rm = na.rm),
                            max    = max(.data$value,                   na.rm = na.rm),
                            mean   = mean(.data$value,                  na.rm = na.rm),
                            sd     = stats::sd(.data$value,             na.rm = na.rm),
                            sum    = sum(.data$value,                   na.rm = na.rm))
}

#' Custom formatting for gt Tables
#'
#' @import gt
#' @param mytab gt object
#' @noRd
mytabstyle <- function(mytab) {
  mytab %>%
    gt::tab_options(data_row.padding = gt::px(0),
                    column_labels.border.top.style = "none",
                    table.border.top.style = "none",
                    table_body.border.top.style = "none",
                    column_labels.font.weight = "bold") %>%
    gt::tab_style(style = cell_text(align = "left", v_align = "top"),
                  locations = cells_body())
}

#' Turn tibble into gt Table with Custom Formatting
#'
#' @description Formatting includes:
#'   * Text align to top/left
#'   * Smaller row padding
#'   * No top border
#'   * Bold column labels
#'
#' @import gt
#'
#' @param df Data frame/tibble
#'
#' @return Formatted gt table
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars %>%
#'   slice(1:5) %>%
#'   mygt()
mygt <- function(df) {
  df %>%
    gt::gt() %>%
    mytabstyle()
}

#' Stratified Descriptive Tables Using gtsummary
#'
#' @import gtsummary gt dplyr
#'
#' @description \code{table1} wraps \code{\link[gtsummary]{tbl_summary}} with some custom options:
#'   * Condensed formatting for better space use.
#'   * Stratification in columns with informative spanning headers, plus an "overall" column if desired.
#'   * First row with total count.
#'   * Rounding of continuous values to one decimal digit by default.
#'
#' @param data Data frame/tibble to print. Required.
#' @param by Optional: Stratification variable for columns.
#' @param overall Optional: Show "Overall" column if stratifying? Defaults to \code{TRUE}.
#' @param label Optional: Label for strata. If empty, the variable label or
#'   variable name of the \code{by} variable will be used.
#' @param digits Optional: Level of precision (decimal digits) for statistics.
#'   By default, all continuous variables are shown with one decimal digits
#'   for each statistic: \code{all_continuous() ~ c(1, 1)}.
#'   Precision can also be changed for individual variables, e.g.:
#'   \code{all_continuous() ~ c(1, 1), a_specific_variable ~ c(2, 2)}
#' @param statistic Optional: Summary statistic to the shown for specific variables, e.g.:
#'   \code{list(a_specific_variable ~ "{min}, {max}")}.
#'   Passed on to \code{\link[gtsummary]{tbl_summary}(statistic = ...)}.
#' @param ... Optional: Passed on to \code{\link[gtsummary]{tbl_summary}}.
#'
#' @return Formatted table. Continuous variables are
#'   median (quartile 1, quartile 3); categorical variables are
#'   count (column percent).
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars %>%
#'   table1(by = "gear")
table1 <- function(data,
                   by        = NULL,
                   overall   = TRUE,
                   label     = "",
                   digits    = list(all_continuous() ~ c(1, 1)),
                   statistic = NULL,
                   ...) {
  options(gtsummary.tbl_summary.percent_fun = function(x) sprintf("%.0f", 100 * x))
  mystats <- list(N ~ "{n}")
  if(!is.null(statistic))
    mystats <- append(mystats, statistic)
  if(is.null(by)) {
    data %>%
      dplyr::mutate(N = 1) %>%
      dplyr::select(.data$N, dplyr::everything()) %>%
      gtsummary::tbl_summary(statistic = mystats, digits = digits, ...) %>%
      gtsummary::bold_labels() %>%
      gtsummary::as_gt(include = -tab_footnote) %>%
      mytabstyle()
  } else {
    if(overall == TRUE) {
      if(sum(is.na(data %>%
                   dplyr::select(var = dplyr::one_of(by)) %>%
                   dplyr::pull(.data$var))) > 0)
        message(paste("Dropping observations where 'by' is missing (NA);",
                      "otherwise gtsummary(overall = TRUE) breaks"))
      res <- data %>%
        dplyr::mutate(N = 1) %>%
        dplyr::select(.data$N, dplyr::everything()) %>%
        dplyr::rename(by = dplyr::one_of(by)) %>%
        dplyr::filter(!is.na(by)) %>%
        labelled::copy_labels_from(from = data) %>%
        gtsummary::tbl_summary(by = by, statistic = mystats, digits = digits, ...) %>%
        gtsummary::add_overall() %>%
        gtsummary::modify_header(stat_0 = "**Overall**", stat_by = "**{level}**") %>%
        gtsummary::bold_labels()
    } else {
      res <- data %>%
        dplyr::mutate(N = 1) %>%
        dplyr::select(.data$N, dplyr::everything()) %>%
        dplyr::rename(by = dplyr::one_of(by)) %>%
        gtsummary::tbl_summary(by = by, statistic = mystats, digits = digits, ...) %>%
        gtsummary::modify_header(stat_by = "**{level}**") %>%
        gtsummary::bold_labels()
    }
    if(label == "")
      label <- paste("By", labelled::var_label(data %>% dplyr::pull(by)))
    if(label == "By ")  # If variable label is empty, use variable name
      label <- paste("By", by)
    res %>%
      gtsummary::as_gt(include = -tab_footnote) %>%
      gt::tab_spanner(label   = gt::md(paste0("**", label, "**")),
                             columns = gt::matches("stat_[123456789]")) %>%
      mytabstyle()
  }
}

#' Event Counts, Person-Time, Incidence Rates, and Cumulative Incidence (Risk).
#'
#' @import survival dplyr tidyr purrr
#' @importFrom stats qnorm
#' @importFrom utils tail
#'
#' @description Summary table of overall or stratified incidence rates and cumulative indicence.
#'   To obtain stratified results, group the tibble before calling \code{rates}.
#'   See examples.
#'
#' @param data Data frame/tibble. If grouped using \code{\link[dplyr]{group_by}},
#'   then stratified results will be returned.
#' @param time Variable indicating follow-up time.
#' @param event Variable indicating events (event = 1, censor = 0).
#' @param conversion Divisor for follow-up time. Defaults to 1.
#'   Set \code{conversion = 12} if \code{time} is in months and
#'   rates per person-years are desired.
#' @param factor Person-time units for the denominator. Defaults to \code{1000},
#'   e.g., for rates per 1000 person-years of follow-up.
#' @param rate_digits Decimal digits to display for rates. Defaults to 1.
#' @param risk_digits Decimal digits to display for
#'   cumulative incidence (risk). Defaults to 1.
#'
#' @return Tibble:
#'   *  \code{n} Count of participants
#'   *  \code{Events} Count of events
#'   *  \code{Person-time} Sum of person-time
#'   *  \code{Rate per person-time} Incidence rate
#'   *  \code{Risk (Kaplan-Meier, %)} Cumulative incidence
#'      at the end of follow-up, with log-log 95% confidence intervals
#'
#' @export
#'
#' @references
#' Greenland S, Rothman KJ (2008). Introduction to Categorical Statistics. In:
#' Rothman KJ, Greenland S, Lash TL. Modern Epidemiology, 3rd edition.
#' Philadelpha, PA: Lippincott Williams & Wilkins. Page 242. (Poisson/large-sample
#' approximation for variance of incidence rates)
#'
#' @examples
#' data(ovarian)  # from survival package
#'
#' # Unstratified
#' ovarian %>%
#'   rates(time = "futime", event = "fustat")
#'
#' # Convert time to person-years
#' ovarian %>%
#'   rates(time = "futime", event = "fustat", conversion = 12)
#'
#' # Stratified by "ecog.ps":
#' ovarian %>%
#'   group_by(ecog.ps) %>%
#'   rates(time = "futime", event = "fustat", conversion = 12)
rates <- function(data, time, event, conversion = 1,
                  rate_digits = 1, risk_digits = 1, factor = 1000) {
  # The helper function is needed because survfit() does not retain grouping of data
  rates_helper <- function(data, time, event, conversion, rate_digits, risk_digits, factor) {
    fit <- survival::survfit(formula = stats::as.formula(paste0("Surv(", time, ", ", event, ") ~ 1")),
                             data    = data, type = "kaplan-meier", conf.type = "log-log")
    cuminc <- 1-c(utils::tail(summary(fit)$surv,  n = 1),
                  utils::tail(summary(fit)$upper, n = 1),
                  utils::tail(summary(fit)$lower, n = 1))
    data %>%
      dplyr::rename(eventvar = dplyr::one_of(event),
                    timevar  = dplyr::one_of(time)) %>%
      dplyr::summarize(
        `n`           = dplyr::n(),
        `Events`      = sum(.data$eventvar),
        `Person-time` = sum(.data$timevar) / conversion,
        ir            = .data$`Events` / .data$`Person-time`,
        # Use Poisson approximation for variance of incidence rate
        # Greenland/Rothman in Modern Epidemiology, 3rd edn, page 242
        ll          = exp(log(.data$ir) - stats::qnorm(0.975) * 1/sqrt(.data$`Events`)),
        ul          = exp(log(.data$ir) + stats::qnorm(0.975) * 1/sqrt(.data$`Events`)),
        `Person-time`     = round(.data$`Person-time`, digits = 0),
        `Rate per person-time` = paste0(
          format(round(.data$ir * factor, digits = rate_digits), nsmall = rate_digits),
          " (",
          format(round(.data$ll * factor, digits = rate_digits), nsmall = rate_digits),
          "-",
          format(round(.data$ul * factor, digits = rate_digits), nsmall = rate_digits),
          ")"),
        `Risk (Kaplan-Meier, %)`    = paste0(
          format(round(cuminc[1] * 100, digits = risk_digits),
                 nsmall = risk_digits),
          " (",
          format(round(cuminc[2] * 100, digits = risk_digits),
                 nsmall = risk_digits),
          "-",
          format(round(cuminc[3] * 100, digits = risk_digits),
                 nsmall = risk_digits),
          ")")) %>%
      dplyr::select(-.data$ir, -.data$ll, -.data$ul)
  }

  if(dplyr::is_grouped_df(data))
    data <- data %>% tidyr::nest()
  else
    data <- data %>% tidyr::nest(data = dplyr::everything())
  data %>%
    dplyr::summarize(results = purrr::map(
      .x = data,
      .f = ~rates_helper(data        = .x,
                         time        = time,
                         event       = event,
                         conversion  = conversion,
                         rate_digits = rate_digits,
                         risk_digits = risk_digits,
                         factor      = factor))) %>%
    tidyr::unnest(col = .data$results)
}
