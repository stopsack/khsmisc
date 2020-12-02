#' Get counts
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param type Type of statistic quested
#' @param factor Factor for rates. Defaults to 1000.
#' @param risk_percent Display risks as percentage?
#' @param risk_digits Number of digits to round risk estimates to
#' @param rate_digits Number of digits to round rate estimates to
#'
#' @return Tibble
#' @noRd
table_counts <- function(data, event, time, outcome,
                         effectmodifier = NULL, effectmodifier_level = NULL,
                         type, factor, risk_percent, risk_digits, rate_digits) {
  if(!missing(effectmodifier) & !missing(effectmodifier_level)) {
    if(!is.null(effectmodifier_level) & !is.null(effectmodifier)) {
      if(!is.na(effectmodifier)) {
        data <- data %>%
          dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
          dplyr::filter(.data$.effectmod %in% effectmodifier_level)
      }
    }
  }

  if(stringr::str_detect(string = type, pattern = "outcomes") |
     stringr::str_detect(string = type, pattern = "risk")) {
    data <- data %>% dplyr::select(.data$.exposure, outcome   = dplyr::one_of(outcome)) %>%
      dplyr::mutate(event = NA, time = NA)
  } else {
    if(stringr::str_detect(string = type, pattern = "events") |
       stringr::str_detect(string = type, pattern = "rate") |
       type == "time") {
      data <- data %>% dplyr::select(.data$.exposure,
                                     event     = dplyr::one_of(event),
                                     time      = dplyr::one_of(time)) %>%
        dplyr::mutate(outcome = NA)
    } else {
      data <- data %>%
        dplyr::select(.data$.exposure) %>%
        dplyr::mutate(event = NA, time = NA, outcome = NA)
    }
  }

  data %>%
    dplyr::group_by(.data$.exposure) %>%
    dplyr::summarize(res = dplyr::case_when(
      type == "outcomes"              ~ paste(sum(.data$outcome)),
      type == "events"                ~ paste(sum(.data$event)),
      type == "time"                  ~
        paste(trimws(format(round(sum(.data$time), digits = 0), nsmall = 0))),
      type == "total"                 ~ paste(n()),
      type == "outcomes/total"        ~ paste(sum(.data$outcome), n(), sep = "/"),
      type == "events/time"           ~
        paste(sum(.data$event),
              trimws(format(round(sum(.data$time), digits = 0), nsmall = 0)),
              sep = "/"),
      type == "events/total"          ~ paste(sum(.data$event), n(), sep = "/"),
      type == "risk"                  ~
        paste0(trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = risk_digits), nsmall = risk_digits)),
               if_else(risk_percent == TRUE, true = "%", false = "")),
      type == "risk (ci)"             ~
        paste0(trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = risk_digits), nsmall = risk_digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), " (",
               trimws(format(round(scoreci(success = sum(.data$outcome),
                                           total = n())$conf.low *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = risk_digits), nsmall = risk_digits)), "-",
               trimws(format(round(scoreci(success = sum(.data$outcome),
                                           total = n())$conf.high *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = risk_digits), nsmall = risk_digits)), ")"),
      type == "rate"                  ~
        trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                            digits = rate_digits), nsmall = rate_digits)),
      type == "rate (ci)"             ~
        paste0(trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                                   digits = rate_digits), nsmall = rate_digits)), " (",
               trimws(format(round(factor * exp(log(sum(.data$event) / sum(.data$time))
                                                - stats::qnorm(0.975) * 1/sqrt(sum(.data$event))),
                                   digits = rate_digits), nsmall = rate_digits)),
               "-",
               trimws(format(round(factor * exp(log(sum(.data$event) / sum(.data$time))
                                                + stats::qnorm(0.975) * 1/sqrt(sum(.data$event))),
                                   digits = rate_digits), nsmall = rate_digits)), ")"),
      type == "outcomes (risk)" ~
        paste0(sum(.data$outcome), " (",
               trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = risk_digits), nsmall = risk_digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), ")"),
      type == "outcomes/total (risk)" ~
        paste0(sum(.data$outcome), "/", n(), " (",
               trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = risk_digits), nsmall = risk_digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), ")"),
      type == "events/time (rate)"    ~
        paste0(sum(.data$event), "/",
               trimws(format(round(sum(.data$time), digits = 0), nsmall = 0)), " (",
               trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                                   digits = rate_digits), nsmall = rate_digits)), ")")),
      .groups = "drop")
}

#' Get hazard ratio and CI from Cox model
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param confounders String of covariates
#'
#' @return Tibble
#' @noRd
table_cox <- function(data, event, time,
                      effectmodifier = NULL, effectmodifier_level = NULL,
                      confounders = "") {
  xlevels <- data %>% dplyr::pull(.data$.exposure) %>% levels()

  if(!missing(effectmodifier) & !missing(effectmodifier_level)) {
    if(!is.null(effectmodifier_level) & !(is.null(effectmodifier) | is.na(effectmodifier))) {
      data <- data %>% dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
        dplyr::filter(.data$.effectmod %in% effectmodifier_level)
    }
  }

  survival::coxph(formula = stats::as.formula(paste0("Surv(", time, ", ", event,
                                                     ") ~ .exposure", confounders)),
        data = data) %>%
    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::select(.data$term, .data$estimate, .data$conf.low, .data$conf.high) %>%
    dplyr::mutate_if(.predicate = is.numeric,
                     .funs = ~trimws(format(round(., digits = 2), nsmall = 2))) %>%
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
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param confounders String of covariates
#'
#' @return Tibble
#' @noRd
table_cox_joint <- function(data, event, time,
                            effectmodifier, effectmodifier_level,
                            confounders = "") {
  if(missing(effectmodifier) | missing(effectmodifier_level))
    stop("Effect modifier and level must be specified for joint model ('hr_joint').")
  if(is.na(effectmodifier) | is.null(effectmodifier) | is.null(effectmodifier_level))
    stop("Effect modifier and level must be specified for joint model ('hr_joint').")
  data <- data %>%
    dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
    dplyr::mutate(.joint = paste(.data$.effectmod, .data$.exposure, sep = "__"))
  xlevels <- data %>% dplyr::pull(.data$.exposure) %>% levels()

  survival::coxph(formula = stats::as.formula(paste0("Surv(", time, ", ",
                                                     event, ") ~ .joint", confounders)),
        data = data) %>%
    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::select(.data$term, .data$estimate, .data$conf.low, .data$conf.high) %>%
    dplyr::mutate_if(.predicate = is.numeric,
                     .funs = ~trimws(format(round(., digits = 2), nsmall = 2))) %>%
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


#' Get RR or RD from 'risks' model
#'
#' @param data Dataset
#' @param outcome Binary outcome variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param confounders String of covariates
#' @param type "rr" or "rd"
#' @param risk_percent Display risks as percentage?
#' @param digits Number of digits to round estimates to
#'
#' @return Tibble
#' @noRd
table_rdrr <- function(data, outcome,
                       effectmodifier = NULL, effectmodifier_level = NULL,
                       confounders = "", type, risk_percent, digits) {
  xlevels <- data %>% dplyr::pull(.data$.exposure) %>% levels()

  if(!missing(effectmodifier) & !missing(effectmodifier_level)) {
    if(!is.null(effectmodifier_level) & !(is.null(effectmodifier) | is.na(effectmodifier))) {
      data <- data %>%
        dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
        dplyr::filter(.data$.effectmod %in% effectmodifier_level)
    }
  }

  if(stringr::str_detect(string = type, pattern = "rr")) {
    res <- risks::riskratio(formula = stats::as.formula(paste(outcome, "~ .exposure", confounders)),
                            data = data) %>%
      broom::tidy(conf.int = TRUE, exponentiate = TRUE)
    reference <- "1 (reference)"
    multiply <- 1
  } else {
    res <- risks::riskdiff(formula = stats::as.formula(paste(outcome, "~ .exposure", confounders)),
                           data = data) %>%
      broom::tidy(conf.int = TRUE)
    reference <- "0 (reference)"
    multiply <- if_else(risk_percent == TRUE, true = 100, false = 1)
  }
  res %>%
    dplyr::select(.data$term, .data$estimate, .data$conf.low, .data$conf.high) %>%
    dplyr::mutate_if(.predicate = is.numeric,
                     .funs = ~trimws(format(round(. * multiply, digits = digits), nsmall = digits))) %>%
    dplyr::filter(stringr::str_detect(string = .data$term, pattern = ".exposure")) %>%
    dplyr::mutate(.exposure = stringr::str_remove(string = .data$term, pattern = ".exposure")) %>%
    dplyr::left_join(x = tibble::tibble(.exposure = xlevels), by = ".exposure") %>%
    dplyr::mutate(res = paste0(.data$estimate, " (", .data$conf.low,
                               dplyr::if_else(stringr::str_detect(string = type, pattern = "rr"),
                                       true = "-",
                                       false = " to "),
                               .data$conf.high, ")"),
                  res = dplyr::if_else(is.na(.data$estimate),
                                       true = reference, false = .data$res)) %>%
    dplyr::select(.data$.exposure, .data$res)
}

#' Get RR or RD from 'risks' model (joint)
#'
#' @description
#' For 'joint' coding of exposure and effect modifier.
#'
#' @param data Dataset
#' @param outcome Binary outcome variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param confounders String of covariates
#' @param type "rr" or "rd"
#' @param risk_percent Display risks as percentage?
#' @param digits Number of digits to round estimates to
#'
#' @return Tibble
#' @noRd
table_rdrr_joint <- function(data, outcome,
                             effectmodifier, effectmodifier_level,
                             confounders = "", type, risk_percent, digits) {
  if(missing(effectmodifier) | missing(effectmodifier_level))
    stop("Effect modifier and level must be specified for joint RR or RD model ('_joint').")
  if(is.na(effectmodifier) | is.null(effectmodifier) | is.null(effectmodifier_level))
    stop("Effect modifier and level must be specified for joint RR or RD model ('_joint').")
  data <- data %>%
    dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
    dplyr::mutate(.joint = paste(.data$.effectmod, .data$.exposure, sep = "__"))
  xlevels <- data %>% dplyr::pull(.data$.exposure) %>% levels()

  if(stringr::str_detect(string = type, pattern = "rr")) {
    res <- risks::riskratio(formula = stats::as.formula(paste(outcome, "~ .joint", confounders)),
                            data = data) %>%
      broom::tidy(conf.int = TRUE, exponentiate = TRUE)
    reference <- "1 (reference)"
    multiply <- 1
  } else {
    res <- risks::riskdiff(formula = stats::as.formula(paste(outcome, "~ .joint", confounders)),
                           data = data) %>%
      broom::tidy(conf.int = TRUE)
    reference <- "0 (reference)"
    multiply <- if_else(risk_percent == TRUE, true = 100, false = 1)
  }
  res %>%
    dplyr::select(.data$term, .data$estimate, .data$conf.low, .data$conf.high) %>%
    dplyr::mutate_if(.predicate = is.numeric,
                     .funs = ~trimws(format(round(. * multiply, digits = digits), nsmall = digits))) %>%
    dplyr::filter(stringr::str_detect(string = .data$term,
                                      pattern = paste0(".joint", effectmodifier_level, "__"))) %>%
    dplyr::mutate(.exposure = stringr::str_remove(string = .data$term,
                                                  pattern = paste0(".joint",
                                                                   effectmodifier_level, "__"))) %>%
    dplyr::left_join(x = tibble::tibble(.exposure = xlevels), by = ".exposure") %>%
    dplyr::mutate(res = paste0(.data$estimate, " (", .data$conf.low,
                               dplyr::if_else(stringr::str_detect(string = type, pattern = "rr"),
                                              true = "-",
                                              false = " to "),
                               .data$conf.high, ")"),
                  res = dplyr::if_else(is.na(.data$estimate),
                                       true = reference, false = .data$res)) %>%
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
fill_cells <- function(data, event, time, outcome, exposure, effect_modifier, stratum, confounders,
                       type, factor, risk_percent, risk_digits, rate_digits) {
  data <- data %>% dplyr::rename(.exposure = dplyr::one_of(exposure))

  # Check that exposure is categorical
  if(!(class(data %>% pull(.data$.exposure))[1] %in% c("factor", "character", "logical")))
    stop(paste0("Exposure variable '", exposure,
                "' is not categorical (factor, character, or logical). Change type."))

  # Check that time and event variable exist, if needed
  if(stringr::str_detect(string = type, pattern = "events") |
     stringr::str_detect(string = type, pattern = "hr") | type %in% c("rate", "time")) {
    if(!(event %in% names(data)))
      stop(paste0("Survival data using type = '", type, "' requested, but event variable '",
                  event, "' is not valid for the dataset."))
    if(!(time %in% names(data)))
      stop(paste0("Survival data using type = '", type, "' requested, but time variable '",
                  time, "' is not valid for the dataset."))
  }

  # Check that outcome variable exists, if needed
  if(stringr::str_detect(string = type, pattern = "outcomes") | type %in% c("risk", "rr", "rd")) {
    if(!(outcome %in% names(data)))
      stop(paste0("Using type = '", type, "' requires binary outcome data, but the outcome variable '",
                  outcome, "' is not valid for the dataset."))
    # Check that outcome is binary
    outcomevar <- data %>% dplyr::select(outcome = dplyr::one_of(outcome)) %>% dplyr::pull(outcome)
    if(!(all(sort(unique(outcomevar)) == c(0, 1)) | all(sort(unique(outcomevar)) == c(FALSE, TRUE))))
      stop(paste0("Outcome variable '", outcome,
                  "' must be binary with levels c(0, 1) or c(FALSE, TRUE)."))
  }

  switch(EXPR = type,
         hr        = table_cox(event, time, effect_modifier,
                               stratum, confounders, data = data),
         hr_joint  = table_cox_joint(event, time, effect_modifier,
                                     stratum, confounders, data = data),
         rr        = table_rdrr(data, outcome, effect_modifier, stratum, confounders, type,
                                risk_percent = FALSE, digits = 2),
         rd        = table_rdrr(data, outcome, effect_modifier, stratum, confounders, type,
                                risk_percent, digits = risk_digits),
         rr_joint  = table_rdrr_joint(data, outcome, effect_modifier, stratum, confounders, type,
                                      risk_percent = FALSE, digits = 2),
         rd_joint  = table_rdrr_joint(data, outcome, effect_modifier, stratum, confounders, type,
                                      risk_percent, digits = risk_digits),
         blank     = data %>% distinct(.data$.exposure) %>% dplyr::mutate(res = ""),
         table_counts(data, event, time, outcome, effect_modifier, stratum, type, factor,
                      risk_percent, risk_digits, rate_digits))
}

#' Table 2: Stratified Result Tables
#'
#' @description This function obtains descriptive
#' and inferential results for binary and survival data in the format
#' of a table stratified by exposure and, if requested, by
#' effect modifiers.
#'
#' @param design Design matrix (data frame) that sets up the table.
#'   See Details.
#' @param data Dataset to be used for all analyses.
#' @param layout Optional. \code{"rows"} uses the \code{design} as rows and
#'   exposure categories as columns. \code{"cols"} is the
#'   opposite: \code{design} as columns and exposure categories as rows.
#'   Defaults to \code{"rows"}.
#' @param factor Optional. Used for \code{type = "rates"}: Factor to multiply
#'   events per person-time by. Defaults to \code{1000}.
#' @param risk_percent Optional. Show risk and risk difference estimates in
#'   percentage points instead of proportions. Defaults to \code{FALSE}.
#' @param risk_digits Optional. Number of decimal digits to show for
#'   rounding of risk and risk difference estimates. Defaults to \code{2}
#'   for \code{risk_percent = FALSE} and to \code{0} for
#'   \code{risk_percent = TRUE}.
#' @param rate_digits Optional. Number of decimal digits to show for rates.
#'   Defaults to \code{1}. (Ratios, i.e., hazard ratios and risk ratios are
#'   always shown with 2 decimal digits.)
#'
#' @details The main input parameter is the dataset \code{design}.
#'   Always required are the columns \code{label}, \code{type}, and
#'   \code{exposure}, as well as \code{outcome} for binary outcomes
#'   or \code{time} and \code{event} for survival outcomes:
#'
#'   *  \code{label} A label for each row (or column).
#'   *  \code{time} The time variable for survival data. Needed for,
#'        e.g., \code{type = "hr"} and \code{type = "rate"}
#'        (i.e., whenever \code{outcome} is not used).
#'   *  \code{event} The event variable for survival data.
#'        Events are typically \code{1}, censored observations \code{0}.
#'        Needed for, e.g., \code{type = "hr"} and \code{type = "rate"}
#'        (i.e., whenever \code{outcome} is not used).
#'   *  \code{outcome} The binary outcome variable for risk/prevalence
#'        data. Must be \code{0}/\code{1} or \code{FALSE}/\code{TRUE}.
#'        Needed for, e.g., \code{type = "rr"} and \code{type = "risk"}
#'        (i.e., whenever \code{event} and \code{time} are not used).
#'   *  \code{exposure} The exposure variable. Must be categorical
#'        (factor or logical).
#'   *  \code{effect_modifier} Optional. An effect modifier variable.
#'        Use \code{NULL} or \code{NA} to leave blank.
#'   *  \code{stratum} Optional. A stratum of the effect modifier.
#'        Use \code{NULL} to leave blank. \code{NA} will evaluate
#'        observations with missing data for the \code{effect_modifier}.
#'   *  \code{confounders} Optional. A string in the format
#'        \code{"+ var1 + var2"} that will be substituted into
#'        into \code{formula = exposure + confounders}.
#'        Use \code{""} (empty string) to leave blank; the default.
#'        For Cox models, can add \code{"+ strata(site)"}
#'        to obtain models with stratification by, e.g., \code{site}.
#'   *  \code{type} The statistic requested (case-insensitive):
#'
#'      * \code{"hr"} Hazard ratio and 95% confidence interval
#'        from Cox proportional hazards regression.
#'      * \code{"rr"} Risk ratio (or prevalence ratio) with
#'        95% confidence interval from \code{\link[risks]{riskratio}}.
#'      * \code{"rd"} Risk difference (or prevalence difference)
#'        with 95% confidence interval from \code{\link[risks]{riskdiff}}.
#'      * \code{"events"} Event count.
#'      * \code{"time"} Person-time.
#'      * \code{"outcomes"} Outcome count.
#'      * \code{"total"} Number of observations.
#'      * \code{"events/time"} Events slash person-time.
#'      * \code{"events/total"} Events slash number of observations.
#'      * \code{"risk"} Risk (or prevalence), i.e., events divided
#'        by number of observations. Change between display as proportion
#'        or percent using the parameter \code{risk_percent}.
#'      * \code{"risk (ci)"} Risk with 95% confidence interval
#'        (Wilson score interval for binomial proportions, see
#'        \code{\link[khsmisc]{scoreci}}).
#'      * \code{"rate"} Event rate: event count divided by person-time,
#'        multiplied by \code{factor}.
#'      * \code{"rate (ci)"} Event rate with 95% confidence interval
#'        (Poisson-type interval, see \code{\link[khsmisc]{rates}}).
#'      * \code{"outcomes (risk)"} A combination: Outcomes
#'        followed by risk in parentheses.
#'      * \code{"outcomes/total (risk)"} A combination: Outcomes slash total
#'        followed by risk in parentheses.
#'      * \code{"events/time (rate)"} A combination: Events slash time
#'        followed by rate in parentheses.
#'      * \code{"blank"} An empty line.
#'
#'      By default, regression models will be fit separately for each
#'      stratum of the \code{effect_modifier}. Append \code{"_joint"}
#'      to \code{"hr"}, \code{"rr"}, or \code{"rd"} to obtain
#'      "joint" models for exposure and effect modifier that have
#'      a single reference category. Example: \code{type = "hr_joint"}.

#'
#' Hint: Use \code{\link[tibble]{tibble}}, \code{\link[tibble]{tribble}}, and
#'   \code{\link[dplyr]{mutate}} to construct the \code{design} dataset, especially
#'   variables that are used repeatedly (e.g., \code{exposure, time, event},
#'   or \code{outcome}). See examples.
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
#'   mutate(rx = factor(rx, levels = 1:2,
#'                      labels = c("Control (CP)", "Treatment (CP+DXR)")),
#'          futime = futime / 365.25)  # presumably years
#'
#' # Example 1: Binary outcomes (use 'outcome' variable)
#' # Set table design
#' design1 <- tibble(label = c("Outcomes",
#'                             "Total",
#'                             "Outcomes/Total",
#'                             "Risk",
#'                             "Risk (CI)",
#'                             "Outcomes (Risk)",
#'                             "Outcomes/Total (Risk)",
#'                             "RR",
#'                             "RD")) %>%
#'   mutate(type = label, exposure = "rx", outcome = "fustat")
#'
#' # Generate table2
#' table2(design = design1, data = ovarian)
#'
#' # Use 'design' as columns (selecting RR and RD only)
#' table2(design = design1 %>% filter(label %in% c("RR", "RD")),
#'        data = ovarian, layout = "cols")
#'
#' # Example 2: Survival outcomes (use 'time' and 'event'),
#' #   with an effect modifier and a confounder
#' # Set table design
#' design2 <- tribble(
#'   # Elements that vary by row:
#'   ~label,                       ~stratum, ~confounders, ~type,
#'   "Overall: Events",            NULL,     "",           "events",
#'   "  Person-years",             NULL,     "",           "time",
#'   "  Rate/1000 py (95% CI)",    NULL,     "",           "rate (ci)",
#'   "  Unadjusted HR (95% CI)",   NULL,     "",           "hr",
#'   "  Age-adjusted HR (95% CI)", NULL,     "+ age",      "hr",
#'   "",                           NULL,     "",           "blank",
#'   "Stratified models",          NULL,     "",           "blank",
#'   "ECOG PS1 (events/N)",        1,        "",           "events/total",
#'   "  Unadjusted",               1,        "",           "hr",
#'   "  Age-adjusted",             1,        "+ age",      "hr",
#'   "ECOG PS2 (events/N)",        2,        "",           "events/total",
#'   "  Unadjusted",               2,        "",           "hr",
#'   "  Age-adjusted",             2,        "+ age",      "hr",
#'   "",                           NULL,     "",           "blank",
#'   "Joint model, age-adj.",      NULL,     "",           "blank",
#'   "  ECOG PS1",                 1,        "+ age",      "hr_joint",
#'   "  ECOG PS2",                 2,        "+ age",      "hr_joint") %>%
#'   # Elements that are the same for all rows:
#'   mutate(exposure = "rx", event = "fustat", time = "futime",
#'          effect_modifier = "ecog.ps")
#'
#' # Generate table2
#' table2(design = design2, data = ovarian)
#'
#' # Get formatted output:
#' \dontrun{
#' table2(design = design2, data = ovarian) %>%
#'   mygt()
#' }
table2 <- function(design, data, layout = "rows", factor = 1000,
                   risk_percent = FALSE,
                   risk_digits = dplyr::if_else(risk_percent == TRUE, true = 0, false = 2),
                   rate_digits = 1) {
  name <- labelled::var_label(dplyr::pull(data, design$exposure[1]))
  if(is.null(name))
    name <- design$exposure[1]

  if(!("event"       %in% names(design))) design$event       <- NA
  if(!("time"        %in% names(design))) design$time        <- NA
  if(!("outcome"     %in% names(design))) design$outcome     <- NA
  if(!("confounders" %in% names(design))) design$confounders <- ""
  if(!("effect_modifier" %in% names(design) & "stratum" %in% names(design)))
    design <- design %>% mutate(effect_modifier = NA, stratum = NA)

  res <- design %>%
    dplyr::mutate(type   = stringr::str_to_lower(string = .data$type),
                  index  = dplyr::row_number(),
                  result = purrr::pmap(.l = list(.data$event, .data$time, .data$outcome,
                                                 .data$exposure,
                                                 .data$effect_modifier, .data$stratum,
                                                 .data$confounders, .data$type),
                                       .f = fill_cells, data = data, factor = factor,
                                       risk_percent = risk_percent,
                                       risk_digits = risk_digits, rate_digits = rate_digits)) %>%
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
