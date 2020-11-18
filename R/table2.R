#' Get counts
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param exposure Exposure variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param type Type of statistic quested
#' @param factor Factor for rates. Defaults to 1000.
#'
#' @return Tibble
#' @noRd
table_counts <- function(data, event, time, exposure,
                         effectmodifier = NULL, effectmodifier_level = NULL,
                         type, factor) {
  if(!missing(effectmodifier) & !missing(effectmodifier_level)) {
    if(!is.null(effectmodifier_level) & !(is.null(effectmodifier) | is.na(effectmodifier))) {
      data <- data %>% dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
        dplyr::filter(.data$.effectmod == effectmodifier_level)
    }
  }

  data <- data %>%
    dplyr::select(.exposure = dplyr::one_of(exposure),
                  event = dplyr::one_of(event), time = dplyr::one_of(time)) %>%
    dplyr::group_by(.data$.exposure)

  data %>% dplyr::summarize(res = dplyr::case_when(
    type == "events"       ~ paste(sum(.data$event)),
    type == "time"         ~ format(round(sum(.data$time), digits = 0), nsmall = 0),
    type == "total"        ~ paste(n()),
    type == "events/time"  ~ paste(sum(.data$event), sum(.data$time), sep = "/"),
    type == "events/total" ~ paste(sum(.data$event), n(), sep = "/"),
    type == "rate"         ~ format(round(sum(.data$event) * factor / n(), digits = 1), nsmall = 1)),
    .groups = "drop")
}

#' Get hazard ratio and CI from Cox model
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param exposure Exposure variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param confounders String of covariates
#'
#' @return Tibble
#' @noRd
table_cox <- function(data, event, time, exposure,
                      effectmodifier = NULL, effectmodifier_level = NULL,
                      confounders = "") {
  data <- data %>% dplyr::rename(.exposure = dplyr::one_of(exposure))
  xlevels <- data %>% dplyr::pull(.data$.exposure) %>% levels()

  if(!missing(effectmodifier) & !missing(effectmodifier_level)) {
    if(!is.null(effectmodifier_level) & !(is.null(effectmodifier) | is.na(effectmodifier))) {
      data <- data %>% dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
        dplyr::filter(.data$.effectmod == effectmodifier_level)
    }
  }

  survival::coxph(formula = stats::as.formula(paste0("Surv(", time, ", ", event,
                                                     ") ~ .exposure", confounders)),
        data = data) %>%
    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::select(.data$term, .data$estimate, .data$conf.low, .data$conf.high) %>%
    dplyr::mutate_if(.predicate = is.numeric,
                     .funs = ~format(round(., digits = 2), nsmall = 2)) %>%
    dplyr::filter(stringr::str_detect(string = .data$term, pattern = ".exposure")) %>%
    dplyr::mutate(.exposure = stringr::str_remove(string = .data$term, pattern = ".exposure")) %>%
    dplyr::left_join(x = tibble::tibble(.exposure = xlevels), by = ".exposure") %>%
    dplyr::mutate(res = paste0(.data$estimate, " (", .data$conf.low, "-", .data$conf.high, ")"),
                  res = dplyr::if_else(is.na(.data$estimate),
                                       true = "1 (reference)", false = .data$res)) %>%
    dplyr::select(.data$.exposure, .data$res)
}

#' Get hazard ratio and CI from Cox model (joint)
#'
#' @description
#' For 'joint' coding of exposure and effect modifier.
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param exposure Exposure variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param confounders String of covariates
#'
#' @return Tibble
#' @noRd
table_cox_joint <- function(data, event, time, exposure,
                            effectmodifier, effectmodifier_level,
                            confounders = "") {
  if(missing(effectmodifier) | missing(effectmodifier_level))
    stop("Effect modifier and level must be specified for joint model ('cox_joint').")
  data <- data %>% dplyr::rename(.exposure = dplyr::one_of(exposure),
                                 .effectmod = dplyr::one_of(effectmodifier)) %>%
    dplyr::mutate(.joint = paste(.data$.effectmod, .data$.exposure, sep = "__"))
  xlevels <- data %>% dplyr::pull(.data$.exposure) %>% levels()

  survival::coxph(formula = stats::as.formula(paste0("Surv(", time, ", ",
                                                     event, ") ~ .joint", confounders)),
        data = data) %>%
    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::select(.data$term, .data$estimate, .data$conf.low, .data$conf.high) %>%
    dplyr::mutate_if(.predicate = is.numeric,
                     .funs = ~format(round(., digits = 2), nsmall = 2)) %>%
    dplyr::filter(stringr::str_detect(string = .data$term,
                                      pattern = paste0(".joint", effectmodifier_level, "__"))) %>%
    dplyr::mutate(.exposure = stringr::str_remove(string = .data$term,
                                                  pattern = paste0(".joint",
                                                                   effectmodifier_level, "__"))) %>%
    dplyr::left_join(x = tibble::tibble(.exposure = xlevels), by = ".exposure") %>%
    dplyr::mutate(res = paste0(.data$estimate, " (", .data$conf.low, "-", .data$conf.high, ")"),
                  res = dplyr::if_else(is.na(.data$estimate),
                                       true = "1 (reference)", false = .data$res)) %>%
    dplyr::select(.data$.exposure, .data$res)
}

#' Select cell fill function
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param exposure Exposure variable
#' @param effectmodifier Effect modifier variable
#' @param stratum Effect modifier level
#' @param confounders String of covariates
#' @param type Type of statistic quested from table_count
#' @param factor Factor for rates. Defaults to 1000.
#'
#' @return Tibble
#' @noRd
fill_cells <- function(data, event, time, exposure, effect_modifier, stratum, confounders, type, factor) {
  switch(EXPR = type,
         cox  = table_cox(event, time, exposure, effect_modifier,
                          stratum, confounders, data = data),
         cox_joint = table_cox_joint(event, time, exposure, effect_modifier,
                                     stratum, confounders, data = data),
         blank = data %>% rename(.exposure = dplyr::one_of(exposure)) %>%
           distinct(.data$.exposure) %>% dplyr::mutate(res = ""),
         table_counts(event, time, exposure, effect_modifier, stratum, type, data = data, factor = factor))
}

#' Table 2: Counts and Cox regression results
#'
#' @description This function obtains descriptive
#' and inferential results for survival analyses in the format
#' of a table stratified by exposure and, if requested, by
#' effect modifiers.
#'
#' @param design Data frame that sets up the table. See Details.
#' @param data Dataset to be used for all analyses.
#' @param layout \code{"rows"} uses the design as rows and
#'   exposure categories as columns. \code{"cols"} is the
#'   opposite: design as columns and exposure categories as rows.
#' @param factor Used for \code{type = "rates"}: Factor to multiply
#'   events per person-time by. Defaults to 1000.
#'
#' @details The main input parameter is the dataset \code{design},
#'   which must consist of the following variables:
#'
#'   *  \code{label} A label for each row (or column).
#'   *  \code{time} The time variable of the survival model.
#'   *  \code{event} The event variable of the survival model.
#'        Events must be \code{1}, censored observations \code{0}.
#'   *  \code{exposure} The exposure variable. Must be categorical
#'        (factor or logical).
#'   *  \code{effect_modifier} An effect modifier variable.
#'        Use \code{NULL} or \code{NA} to leave blank.
#'   *  \code{stratum} A stratum of the effect modifier.
#'        Use \code{NULL} to leave blank. (\code{NA} will evaluate
#'        observations with missing effect modifier.)
#'   *  \code{confounders} A string in the format
#'        \code{"+ confounder1 + confounder2"} that will be substituted into
#'        into \code{coxph(formula = exposure + confounders)}.
#'        Use \code{""} (empty string) to leave blank. Can add \code{"+ strata(site)"}
#'        to obtain Cox models with stratification by, e.g., \code{site}.
#'   *  \code{type} The statistic requested:
#'
#'      * \code{"cox"} Hazard ratio and 95% confidence interval.
#'        Models will be fit separately for each stratum of
#'        the \code{effect_modifier}.
#'      * \code{"cox_joint"} Hazard ratio and 95% confidence interval.
#'        A 'joint' model for exposure and effect modifier with a
#'        single reference category will be fit.
#'      * \code{"events"} Event count
#'      * \code{"time"} Person-time
#'      * \code{"total"} Number of observations
#'      * \code{"events/time"} Events slash person-time
#'      * \code{"events/total"} Events slash number of observations
#'      * \code{"rate"} Event rate: event count divided by person-time
#'      * \code{"blank"} An empty line
#'
#' Hint: Use \code{\link[tibble]{tibble}}, \code{\link[tibble]{tribble}}, and
#'   \code{\link[dplyr]{mutate}} to construct the \code{design} dataset, especially
#'   variables that are used repeatedly (e.g., \code{time, event}). See examples.
#'
#' @return Tibble. Get formatted output by passing on to
#'   \code{\link[khsmisc]{mygt}}.
#' @export
#'
#' @examples
#' # Load 'ovarian' dataset from survival package
#' data(ovarian)
#'
#' # The exposure (here, 'rx') must be categorical
#' ovarian <- ovarian %>%
#'   as_tibble() %>%
#'   mutate(rx = factor(rx),
#'          futime = futime / 365.25)  # presumably years
#'
#' # Set design elements that vary by row
#' tribble(
#'    ~label,                      ~stratum, ~confounders, ~type,
#'   "Overall: Events",            NULL,     "",           "events",
#'   "Overall: Person-years",       NULL,     "",           "time",
#'   "Overall: Rate/1000 years",    NULL,     "",           "rate",
#'   "  Unadjusted HR (95% CI)",   NULL,     "",           "cox",
#'   "  Age-adjusted HR (95% CI)", NULL,     "+ age",      "cox",
#'   "",                           NULL,     "",           "blank",
#'   "Stratified models",          NULL,     "",           "blank",
#'   "ECOG PS1 (events/N)",        1,        "",           "events/total",
#'   "  Unadjusted",               1,        "",           "cox",
#'   "  Age-adjusted",             1,        "+ age",      "cox",
#'   "ECOG PS2 (events/N)",        2,        "",           "events/total",
#'   "  Unadjusted",               2,        "",           "cox",
#'   "  Age-adjusted",             2,        "+ age",      "cox",
#'   "",                           NULL,     "",           "blank",
#'   "Joint model, age-adj.",      NULL,     "",           "blank",
#'   "  ECOG PS1",                 1,        "+ age",      "cox_joint",
#'   "  ECOG PS2",                 2,        "+ age",      "cox_joint") %>%
#' # Set design elements that are the same for all rows
#'   mutate(exposure = "rx", event = "fustat", time = "futime",
#'         effect_modifier = "ecog.ps") %>%
#' # Generate table2
#'   table2(data = ovarian)
#'
#' # Use '%>% mygt()' to get formatted version (not shown)
table2 <- function(design, data, layout = "rows", factor = 1000) {
  name <- labelled::var_label(dplyr::pull(data, design$exposure[1]))
  if(is.null(name))
    name <- design$exposure[1]

  res <- design %>%
    dplyr::mutate(index = dplyr::row_number(),
                  result = purrr::pmap(.l = list(.data$event, .data$time, .data$exposure,
                                                 .data$effect_modifier, .data$stratum,
                                                 .data$confounders, .data$type),
                                       .f = fill_cells, data = data, factor = factor)) %>%
    dplyr::select(.data$index, .data$label, .data$result) %>%
    tidyr::unnest(cols = .data$result)
  if(layout == "rows")
    res %>% tidyr::pivot_wider(names_from = .data$.exposure, values_from = .data$res) %>%
    dplyr::rename(!!name := .data$label) %>%
    dplyr::select(-.data$index)
  else {
    if(sum(duplicated(design$label)) > 0 | "" %in% design$label) {
      res %>%
        tidyr::pivot_wider(names_from = c(.data$index, .data$label), values_from = .data$res)
    } else {
      res %>%
        dplyr::select(-.data$index) %>%
        tidyr::pivot_wider(names_from = .data$label, values_from = .data$res) %>%
        dplyr::rename(!!name := .data$.exposure)
    }
  }
}
