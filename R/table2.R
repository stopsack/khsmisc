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
#' @param digits Number of digits to round estimates to
#' @param to Separator for mean/difference confidence intervals
#'
#' @return Tibble
#' @noRd
table_counts <- function(data, event, time, outcome,
                         effectmodifier = NULL, effectmodifier_level = NULL,
                         type, factor, risk_percent,
                         digits, to) {
  if(!missing(effectmodifier) & !missing(effectmodifier_level)) {
    if(!is.null(effectmodifier_level) & !is.null(effectmodifier)) {
      if(!is.na(effectmodifier)) {
        data <- data %>%
          dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
          dplyr::filter(.data$.effectmod %in% effectmodifier_level)
      }
    }
  }

  if(stringr::str_detect(string = type, pattern = "outcomes|risk|mean|median")) {
    data <- data %>% dplyr::select(.data$.exposure, outcome = dplyr::one_of(outcome)) %>%
      dplyr::mutate(event = NA, time = NA)
  } else {
    if(stringr::str_detect(string = type, pattern = "events|rate") |
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
  if(is.null(to)) {
    if(stringr::str_detect(string = type, pattern = "mean|median"))
      to <- " to "
    else
      to <- "-"
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
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = "")),
      type == "risk (ci)"             ~
        paste0(trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), " (",
               trimws(format(round(scoreci(success = sum(.data$outcome),
                                           total = n())$conf.low *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = digits), nsmall = digits)), to,
               trimws(format(round(scoreci(success = sum(.data$outcome),
                                           total = n())$conf.high *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = digits), nsmall = digits)), ")"),
      type == "rate"                  ~
        trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                            digits = digits), nsmall = digits)),
      type == "rate (ci)"             ~
        paste0(trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                                   digits = digits), nsmall = digits)), " (",
               trimws(format(round(factor * exp(log(sum(.data$event) / sum(.data$time))
                                                - stats::qnorm(0.975) * 1/sqrt(sum(.data$event))),
                                   digits = digits), nsmall = digits)),
               to,
               trimws(format(round(factor * exp(log(sum(.data$event) / sum(.data$time))
                                                + stats::qnorm(0.975) * 1/sqrt(sum(.data$event))),
                                   digits = digits), nsmall = digits)), ")"),
      type == "outcomes (risk)" ~
        paste0(sum(.data$outcome), " (",
               trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), ")"),
      type == "outcomes/total (risk)" ~
        paste0(sum(.data$outcome), "/", n(), " (",
               trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE, true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), ")"),
      type == "events/time (rate)"    ~
        paste0(sum(.data$event), "/",
               trimws(format(round(sum(.data$time), digits = 0), nsmall = 0)), " (",
               trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                                   digits = digits), nsmall = digits)), ")"),
      type == "mean" ~
        trimws(format(round(mean(.data$outcome), digits = digits), nsmall = digits)),
      type == "mean (ci)" ~
        paste0(trimws(format(round(mean(.data$outcome), digits = digits),
                             nsmall = digits)), " (",
               trimws(format(round(mean(.data$outcome) - stats::qnorm(0.975) *
                                     sqrt(var(.data$outcome) / sum(!is.na(.data$outcome))),
                                   digits = digits),
                             nsmall = digits)),
               to,
               trimws(format(round(mean(.data$outcome) + stats::qnorm(0.975) *
                                     sqrt(var(.data$outcome) / sum(!is.na(.data$outcome))),
                                   digits = digits),
                             nsmall = digits)), ")"),
      type == "median" ~
        trimws(format(round(median(.data$outcome), digits = digits), nsmall = digits)),
      type == "median (iqr)" ~
        paste0(trimws(format(round(median(.data$outcome, na.rm = TRUE),
                                   digits = digits), nsmall = digits)),
               " (",
               trimws(format(round(stats::quantile(.data$outcome, probs = 0.25, na.rm = TRUE),
                                   digits = digits), nsmall = digits)),
               to,
               trimws(format(round(stats::quantile(.data$outcome, probs = 0.75, na.rm = TRUE),
                                   digits = digits), nsmall = digits)), ")")),
      .groups = "drop")
}

#' Get point estimate and CI from regression models
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param outcome Outcome variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param confounders String of covariates
#' @param risk_percent Display risk differences as percentage?
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#'
#' @return Tibble
#' @noRd
table_regress <- function(data, estimand, event, time, outcome,
                          effectmodifier = NULL, effectmodifier_level = NULL,
                          confounders = "", risk_percent = FALSE, digits = 2,
                          to = "-") {
  xlevels <- data %>% dplyr::pull(.data$.exposure) %>% levels()

  if(stringr::str_detect(string = estimand, pattern = "_joint")) {
    if(missing(effectmodifier) | missing(effectmodifier_level))
      stop(paste0("Effect modifier and stratum must be specified for joint model ('",
                  estimand, "')."))
    if(is.na(effectmodifier) | is.null(effectmodifier) | is.null(effectmodifier_level))
      stop(paste0("Effect modifier and stratum must be specified for joint model ('",
                  estimand, "')."))
    pattern <- paste0(".exposure[:digit:]{1,2}__", effectmodifier_level,
                      "__[:digit:]{1,2}__")
    data <- data %>%
      dplyr::rename(.effectmod = dplyr::one_of(effectmodifier))
    xlevels_indices <- 1:length(xlevels)
    names(xlevels_indices) <- xlevels
    emlevels <- data %>% dplyr::pull(.data$.effectmod) %>% factor() %>% levels()
    emlevels_indices <- 1:length(emlevels)
    names(emlevels_indices) <- emlevels
    data <- data %>%
      dplyr::mutate(.exposure = paste(emlevels_indices[.data$.effectmod],
                                      .data$.effectmod,
                                      xlevels_indices[.data$.exposure],
                                      .data$.exposure,
                                      sep = "__"))
  } else {
    pattern <- ".exposure"
    if(!missing(effectmodifier) & !missing(effectmodifier_level)) {
      if(!is.null(effectmodifier_level) & !(is.null(effectmodifier) | is.na(effectmodifier))) {
        data <- data %>% dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
          dplyr::filter(.data$.effectmod %in% effectmodifier_level)
      }
    }
  }

  multiply <- 1
  fit <- switch(EXPR = estimand,
                hr_joint =,
                hr = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  survival::coxph(formula = stats::as.formula(
                    paste0("Surv(", time, ", ", event, ") ~ .exposure", confounders)),
                    data = data)
                },
                rr_joint =,
                rr = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  risks::riskratio(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    data = data)
                },
                rd_joint =,
                rd = {
                  reference <- 0
                  exponent <- FALSE
                  multiply <- if_else(risk_percent == TRUE, true = 100, false = 1)
                  to <- dplyr::if_else(is.null(to), true = " to ", false = to)
                  risks::riskdiff(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    data = data)
                },
                diff_joint =,
                diff = {
                  reference <- 0
                  exponent <- FALSE
                  to <- dplyr::if_else(is.null(to), true = " to ", false = to)
                  stats::lm(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    data = data)
                },
                irr_joint =,
                irr = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  stats::glm(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    family = stats::poisson(link = "log"),
                    data = data)
                },
                fold_joint =,
                fold = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  stats::glm(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    family = stats::gaussian(link = "log"),
                    data = data)
                },
                stop(paste0("Estimand '", estimand, "' is not implemented.")))

  fit %>%
    broom::tidy(conf.int = TRUE, exponentiate = exponent) %>%
    dplyr::select(.data$term, .data$estimate, .data$conf.low, .data$conf.high) %>%
    dplyr::mutate_if(.predicate = is.numeric,
                     .funs = ~trimws(format(round(. * multiply, digits = digits),
                                            nsmall = digits))) %>%
    dplyr::filter(stringr::str_detect(string = .data$term, pattern = pattern)) %>%
    dplyr::mutate(.exposure = stringr::str_remove(string = .data$term, pattern = pattern)) %>%
    dplyr::left_join(x = tibble::tibble(.exposure = xlevels), by = ".exposure") %>%
    dplyr::mutate(res = paste0(.data$estimate, " (", .data$conf.low, to, .data$conf.high, ")"),
                  res = dplyr::if_else(is.na(.data$estimate),
                                       true = paste(reference, "(reference)"),
                                       false = .data$res)) %>%
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
#' @param risk_percent Show risks and risk differences as percentages?
#' @param diff_digits Number of digits to round difference estimates to
#' @param ratio_digits Number of digits to round ratio estimates to
#' @param rate_digits Number of digits to round rate estimates to
#' @param to Separator for mean/difference confidence intervals.
#'
#' @return Tibble
#' @noRd
fill_cells <- function(data, event, time, outcome, exposure, effect_modifier, stratum, confounders,
                       type, factor, risk_percent, diff_digits, ratio_digits, rate_digits, to) {
  data <- data %>% dplyr::rename(.exposure = dplyr::one_of(exposure))

  # Check that exposure is categorical
  if(!(class(data %>% pull(.data$.exposure))[1] %in% c("factor", "character", "logical")))
    warning(paste0("Exposure variable '", exposure,
                   "' is not categorical (factor, character, or logical). ",
                   "Its type was changed to 'factor' but the result may be undesirable, e.g., ",
                   "if the variable is actually continuous and thus has many levels."))
  data$.exposure <- factor(data$.exposure)

  if(type == "" | type == "blank")
    return(data %>% distinct(.data$.exposure) %>% dplyr::mutate(res = ""))

  # Check that time and event variable exist, if needed
  if(stringr::str_detect(string = type, pattern = "events|hr|rate|time")) {
    if(!(event %in% names(data)))
      stop(paste0("Survival data using type = '", type, "' requested, but event variable '",
                  event, "' is not valid for the dataset."))
    if(!(time %in% names(data)))
      stop(paste0("Survival data using type = '", type, "' requested, but time variable '",
                  time, "' is not valid for the dataset."))
  }

  # Check that outcome variable exists, if needed
  if(stringr::str_detect(string = type,
                         pattern = "outcomes|diff|mean|median|risk|rr|rd|irr|fold")) {
    if(!(outcome %in% names(data)))
      stop(paste0("Using type = '", type, "' requires an outcome variable, but the variable '",
                  outcome, "' is not valid for the dataset."))
    # Check that outcome is binary
    outcomevar <- data %>%
      dplyr::select(outcome = dplyr::one_of(outcome)) %>%
      dplyr::pull(outcome)
    if(!(stringr::str_detect(string = type, pattern = "diff|mean|median|irr|fold")))
      if(!(all(sort(unique(outcomevar)) == c(0, 1)) |
           all(sort(unique(outcomevar)) == c(FALSE, TRUE))))
        stop(paste0("Outcome variable '", outcome,
                    "' must be binary with levels c(0, 1) or c(FALSE, TRUE)."))
  }

  # extract desired digits from individual "type" fields
  if(stringr::str_detect(string = type, pattern = "(ci)") |
     stringr::str_detect(string = type, pattern = "(iqr)")) {
    splitted <- stringr::str_split(string = type, pattern = " ",
                                   n = 3, simplify = TRUE)
    type <- paste(splitted[1], splitted[2], collapse = " ", sep = " ")
    indiv_dig <- splitted[3]
  } else {
    splitted <- stringr::str_split(string = type, pattern = " ",
                                   n = 2, simplify = TRUE)
    type <- splitted[1]
    indiv_dig <- splitted[2]
  }
  if(indiv_dig != "" & !is.na(indiv_dig) &
     !is.na(suppressWarnings(as.numeric(indiv_dig)))) {
    digits <- as.numeric(indiv_dig)
  } else {
    digits <- dplyr::case_when(stringr::str_detect(string = type,
                                                   pattern = "hr|rr|irr|fold") ~
                                 ratio_digits,
                               stringr::str_detect(string = type,
                                                   pattern = "rd|diff|mean|risk|outcomes|median") ~
                                 diff_digits,
                               stringr::str_detect(string = type,
                                                   pattern = "rate") ~
                                 rate_digits,
                               TRUE ~ 4)
  }

  if(stringr::str_detect(string = type, pattern = "hr|rr|rd|irr|fold|diff"))
    table_regress(data = data,
                  estimand = type,
                  event = event,
                  time = time,
                  outcome = outcome,
                  effectmodifier = effect_modifier,
                  effectmodifier_level = stratum,
                  confounders = confounders,
                  digits = digits,
                  risk_percent = risk_percent,
                  to = to)
  else
    table_counts(data = data,
                 event = event,
                 time = time,
                 outcome = outcome,
                 effectmodifier = effect_modifier,
                 effectmodifier_level = stratum,
                 type = type,
                 factor = factor,
                 digits = digits,
                 risk_percent = risk_percent,
                 to = to)
}

#' Table 2: Stratified Result Tables
#'
#' @description This function displays descriptive
#' and inferential results for binary, continuous, and survival data
#' in the format of a table stratified by exposure and, if requested, by
#' effect modifiers.
#'
#' This function is intended only for tabulations of final results.
#' Models diagnostics for regression models need to be conducted separately.
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
#' @param diff_digits Optional. Number of decimal digits to show for
#'   rounding of means, risks, and risk difference estimates. Defaults to \code{2}
#'   for \code{risk_percent = FALSE} and to \code{0} for
#'   \code{risk_percent = TRUE}. Can override for each line in \code{type}.
#' @param ratio_digits Optional. Number of decimal digits to show for ratio
#'   estimates. Defaults to \code{2}. Can override for each line in \code{type}.
#' @param rate_digits Optional. Number of decimal digits to show for rates.
#'   Defaults to \code{1}. Can override for each line in \code{type}.
#' @param to Optional. Separator between the lower and the upper bound
#'   of the 95% confidence interval (and interquartile range for medians).
#'   Defaults to \code{" to "} for means, medians, and mean differences;
#'   defaults to \code{"-"} otherwise.
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
#'   *  \code{outcome} The outcome variable for non-survival data
#'        (i.e., whenever \code{event} and \code{time} are not used).
#'        For risk/prevalence data, this variable must be \code{0}/\code{1}
#'        or \code{FALSE}/\code{TRUE}.
#'   *  \code{exposure} The exposure variable. Must be categorical
#'        (factor or logical).
#'   *  \code{effect_modifier} Optional. A categorical effect modifier variable.
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
#'        For Poisson models, can add \code{"+ offset(log(variable))"}
#'        to define the offset.
#'   *  \code{type} The statistic requested (case-insensitive):
#'
#'      Comparative estimates from regression models
#'      (all with 95% confidence intervals):
#'
#'      * \code{"hr"} Hazard ratio from Cox proportional
#'        hazards regression.
#'      * \code{"rr"} Risk ratio (or prevalence ratio)
#'        from \code{\link[risks]{riskratio}}.
#'      * \code{"rd"} Risk difference (or prevalence difference)
#'        from \code{\link[risks]{riskdiff}}.
#'      * \code{"irr"} Incidence rate ratio for count outcomes
#'        from Poisson regression model.
#'      * \code{"diff"} Mean difference from linear model.
#'      * \code{"fold"} Fold change from generalized linear
#'        model with log link.
#'
#'      Absolute estimates per exposure category:
#'
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
#'      * \code{"mean"} Mean.
#'      * \code{"mean (ci)"} Mean and 95% CI.
#'      * \code{"median"} Median.
#'      * \code{"median (iqr)"} Median and interquartile range.
#'      * \code{"blank"} or \code{""} An empty line.
#'
#'      By default, regression models will be fit separately for each
#'      stratum of the \code{effect_modifier}. Append \code{"_joint"}
#'      to \code{"hr"}, \code{"rr"}, \code{"rd"}, \code{"irr"}, \code{"diff"},
#'      or \code{"fold"} to obtain "joint" models for exposure and
#'      effect modifier that have a single reference category.
#'      Example: \code{type = "hr_joint"}. The reference categories for
#'      exposure and effect modifier are their first factor levels, which
#'      can be changed using \code{\link[forcats]{fct_relevel}}.
#'
#'      Digits for rounding estimates can be specified for each line separately.
#'      Example: \code{type = "diff (ci) 3"} to request a mean difference
#'      and its 95% CI rounded to 3 decimal digits.
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
#'   "",                           NULL,     "",           "",
#'   "Joint model, age-adj.",      NULL,     "",           "",
#'   "  ECOG PS1",                 1,        "+ age",      "hr_joint",
#'   "  ECOG PS2",                 2,        "+ age",      "hr_joint") %>%
#'   # Elements that are the same for all rows:
#'   mutate(exposure = "rx", event = "fustat", time = "futime",
#'          effect_modifier = "ecog.ps")
#'
#' # Generate table2
#' table2(design = design2, data = ovarian)
#'
#' # Example 3: Continuous outcomes (use 'outcome' variable);
#' # request rounding to 1 decimal digit in some cases.
#' tribble(~label,                   ~stratum, ~type,
#'         "Marginal mean (95% CI)", 1:2,      "mean (ci) 1",
#'         "resid.ds = 1",           1,        "mean",
#'         "resid.ds = 2",           2,        "mean",
#'         "",                       NULL,     "",
#'         "Stratified model",       NULL,     "",
#'         "  resid.ds = 1",         1,        "diff 1",
#'         "  resid.ds = 2",         2,        "diff 1",
#'         "",                       NULL,     "",
#'         "Joint model",            NULL,     "",
#'         "  resid.ds = 1",         1,        "diff_joint",
#'         "  resid.ds = 2",         2,        "diff_joint") %>%
#'   mutate(exposure = "ecog.ps", outcome = "age",
#'          effect_modifier = "resid.ds") %>%
#'   table2(data = ovarian %>% mutate(ecog.ps = factor(ecog.ps)))
#'
#' # Get formatted output:
#' \dontrun{
#' table2(design = design2, data = ovarian) %>%
#'   mygt()
#' }
table2 <- function(design, data, layout = "rows", factor = 1000,
                   risk_percent = FALSE,
                   diff_digits = dplyr::if_else(risk_percent == TRUE, true = 0, false = 2),
                   ratio_digits = 2, rate_digits = 1, to = NULL) {
  name <- labelled::var_label(dplyr::pull(data, design$exposure[1]))
  if(is.null(name))
    name <- design$exposure[1]

  if(!("event"       %in% names(design))) design$event       <- NA
  if(!("time"        %in% names(design))) design$time        <- NA
  if(!("outcome"     %in% names(design))) design$outcome     <- NA
  if(!("confounders" %in% names(design))) design$confounders <- ""
  if(!("effect_modifier" %in% names(design) & "stratum" %in% names(design)))
    design <- design %>% dplyr::mutate(effect_modifier = NA, stratum = NA)

  res <- design %>%
    dplyr::mutate(type   = stringr::str_to_lower(string = .data$type),
                  index  = dplyr::row_number(),
                  result = purrr::pmap(.l = list(.data$event, .data$time, .data$outcome,
                                                 .data$exposure,
                                                 .data$effect_modifier, .data$stratum,
                                                 .data$confounders, .data$type),
                                       .f = fill_cells, data = data, factor = factor,
                                       risk_percent = risk_percent,
                                       diff_digits = diff_digits,
                                       ratio_digits = ratio_digits,
                                       rate_digits = rate_digits,
                                       to = to)) %>%
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
