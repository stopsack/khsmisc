# Functions for (descriptive) tables

#' Contigency Table: Tabulate Rows Times Columns
#'
#' @import tidyverse purrr dplyr forcats
#' @importFrom rlang `:=`
# @import dplyr forcats tidyr purrr
#'
#' @description Tabulates \code{row} variable times \code{col} variable.
#'   Supports tidy evaluation for \code{row} and \code{col}; see examples.
#'
#' @param data Data frame (tibble).
#' @param row Row variable.
#' @param col Column variable.
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
#'   tabulate_rowcol(gear, carb)
#'
#' # Swap rows and columns, turn off totals:
#' mtcars %>%
#'   tabulate_rowcol(row = carb, col = gear,
#'                   rowsum = FALSE, colsum = FALSE)
tabulate_rowcol <- function(data,
                            row,
                            col,
                            maxlevels = 30,
                            rowsums   = TRUE,
                            colsums   = TRUE) {
  if(utils::packageVersion("forcats") >= "1.0.0")
    fct_expl_na <- forcats::fct_na_value_to_level
  else
    fct_expl_na <- forcats::fct_explicit_na

  topname <- paste(colnames(data %>% dplyr::select({{ row }}))[1], "X",
                   colnames(data %>% dplyr::select({{ col }}))[1])
  data <- data %>%
    dplyr::rename(rowvar = {{ row }}, colvar = {{ col }})
  var1lvls <- length(levels(factor(data$rowvar)))
  var2lvls <- length(levels(factor(data$colvar)))
  if(var2lvls > maxlevels)
    stop(paste("No table is printed because the column variable has", var2lvls,
               "levels. Maximum is set to", maxlevels))
  if(var1lvls > maxlevels)
    stop(paste("No table is printed because the row variable has", var1lvls,
               "levels. Maximum is set to", maxlevels))

  result <- data %>%
    dplyr::mutate_at(.vars = dplyr::vars(.data$rowvar, .data$colvar),
                     .funs = as.factor) %>%
    dplyr::mutate_at(.vars = dplyr::vars(.data$rowvar, .data$colvar),
                     .funs = fct_expl_na) %>%
    dplyr::group_by_at(.vars = dplyr::vars(.data$rowvar, .data$colvar)) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::pivot_wider(names_from = .data$colvar, values_from = .data$n) %>%
    dplyr::ungroup() %>%
    dplyr::rename({{ topname }} := 1) %>%
    dplyr::mutate_at(.vars = dplyr::vars(-{{ topname }}), .funs = ~ifelse(is.na(.), 0, .))
  if(rowsums == TRUE) {
    result <- result %>%
      dplyr::mutate_at(.vars = dplyr::vars({{ topname }}), .funs = as.character)
    result <- result %>%
      dplyr::bind_rows(dplyr::summarize_all(result, .funs = function(x) {
        if(is.numeric(x)) sum(x) else "Total" }))
  }
  if(colsums == TRUE)
    result <- result %>%
    dplyr::mutate(Total = purrr::pmap_dbl(.l = dplyr::select(result, -{{ topname }}),
                                          .f = sum))
  result
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
#' @param ... Optional. Variables to summarize. If not provided,
#'   all numeric variables will be summarized. Supports tidy evaluation;
#'   see examples.
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
#'   tsummary()
#'
#' # Select specific variables and
#' # remove some summary statistics:
#' mtcars %>%
#'   tsummary(mpg, cyl, hp, am, gear, carb) %>%
#'   dplyr::select(-mean, -sd, -sum)
#'
#' # Stratify by 'gear':
#' mtcars %>%
#'   tsummary(mpg, hp, carb, by = gear)
#'
#' # Stratify by 'gear' and 'am':
#' mtcars %>%
#'   tsummary(mpg, hp, carb, by = c(am, gear))
tsummary <- function(data,
                     ...,
                     by    = NULL,
                     na.rm = TRUE) {
  data <- data %>% labelled::remove_labels()
  by <- data %>% dplyr::select({{ by }}) %>% names()
  newdata <- data %>% dplyr::select(!!!rlang::enquos(...), dplyr::all_of(by))
  if(ncol(newdata) == ncol(data %>% dplyr::select(dplyr::all_of(by))))
    newdata <- data
  data <- newdata

  if(is.null(by)) {
    if(data %>%
       dplyr::select_if(is.numeric) %>%
       ncol() == 0)
      stop("No numeric variables in the dataset as requested.")
    data <- data %>%
      dplyr::select_if(is.numeric) %>%
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "variable",
                          values_to = "value") %>%
      dplyr::group_by(.data$variable)
  } else {
    to_keep <- c(data %>% dplyr::select_if(is.numeric) %>% names(),
                 dplyr::all_of(by))
    if(data %>%
       dplyr::select(-dplyr::all_of(by)) %>%
       dplyr::select_if(is.numeric) %>%
       ncol() == 0)
      stop("No numeric variables in the dataset as requested.")
    data <- data %>%
      dplyr::select(dplyr::all_of(to_keep)) %>%
      tidyr::pivot_longer(cols = c(-dplyr::all_of(by)),
                          names_to = "variable",
                          values_to = "value") %>%
      dplyr::group_by_at(dplyr::vars(dplyr::one_of("variable"),
                                     dplyr::all_of(by)))
  }
  data %>% dplyr::summarize(
    rows   = dplyr::n(),
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
#' @param mytab gt object
#' @noRd
mytabstyle <- function(mytab) {
  mytab %>%
    gt::tab_options(data_row.padding = gt::px(0),
                    column_labels.border.top.style = "none",
                    table.border.top.style = "none",
                    table_body.border.top.style = "none",
                    column_labels.font.weight = "bold") %>%
    gt::tab_style(style = gt::cell_text(align = "left",
                                        v_align = "top"),
                  locations = gt::cells_body()) %>%
    gt::tab_style(style = cell_text(align = "left",
                                    v_align = "bottom"),
                  locations = gt::cells_column_labels())
}

#' Turn tibble into gt Table with Custom Formatting
#'
#' @description Formatting includes:
#'   * Text align to top/left
#'   * Smaller row padding
#'   * No top border
#'   * Bold column labels
#'
#' @param df Data frame/tibble
#' @param md Optional. If not \code{NULL}, then the given
#'   columns will be printed with markdown formatting, e.g., \code{md = c(1, 3)}
#'   for columns 1 and 3.
#' @param indent Ignored.
#' @param remove_border Optional. For indented lines, remove the upper
#'   horizontal border line? Defaults to \code{TRUE}.
#'
#' @return Formatted gt table
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars %>%
#'   dplyr::slice(1:5) %>%
#'   mygt()
#'
#' @section Example Output:
#' \if{html}{\figure{mygt.png}{options: width=50\%}}
mygt <- function(df, md = NULL, indent = NULL, remove_border = TRUE) {
  # RMarkdown "output: github_document" cannot handle HTML styles
  if(any(stringr::str_detect(
    string = c("", knitr::opts_knit$get("rmarkdown.pandoc.to")),
    pattern = "gfm"))) {
    res <- knitr::kable(df)
    attr(x = res, which = "mydata") <- df
    return(res)
  } else {
    df_gt <- df %>%
      gt::gt() %>%
      mytabstyle()
    if(!is.null(md)) {
      df_gt <- df_gt %>%
        gt::fmt_markdown(columns = md)
    }
    df_gt
  }
}

#' Alternative footnote for gt tables exported as kable
#'
#' This function can be called on a gt table and will the function just as
#' \code{\link[gt]{tab_footnote}}. If called on a kable table, it will
#' print the kable table, ignoring the footnote.
#'
#' @param data gt table or kable table
#' @param footnote Footnote. Will be ignored for a kable table.
#' @param locations Location, as per gt, e.g., \code{\link[gt]{cells_body}}.
#'   Will be ignored for a kable table.
#' @param placement Where to affix footnote marks. Will be ignored for a kable
#'   table.
#'
#' @return gt table or kable table, whichever format \code{data} is in.
#' @export
tab_footnote <- function(
    data,
    footnote,
    locations = NULL,
    placement = c("auto", "right", "left")) {
  if(inherits(x = data, what = "gt_tbl")) {
    gt::tab_footnote(
      data = data,
      footnote = footnote,
      locations = locations,
      placement = placement)
  } else {
    data
  }
}

#' Alternative styling for gt tables exported as kable
#'
#' This function can be called on a gt table and will function just as
#' \code{\link[gt]{tab_footnote}}. If called on a kable table, it will
#' print the kable table, ignoring the styling.
#'
#' @param data gt table or kable table
#' @param style Style definition, e.g., per \code{\link[gt]{cell_text}}. Will
#'   be ignored for a kable table.
#' @param locations Location, as per gt, e.g., \code{\link[gt]{cells_body}}.
#'   Will be ignored for a kable table.
#'
#' @return gt table or kable table, whichever format \code{data} is in.
#' @export
tab_style <- function(data, style, locations) {
  if(inherits(x = data, what = "gt_tbl")) {
    gt::tab_style(
      data = data,
      style = style,
      locations = locations)
  } else {
    data
  }
}
