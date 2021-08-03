#' Get counts
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param time2 Optional second time variable
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
table_counts <- function(data, event, time, time2, outcome,
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

  if(stringr::str_detect(string = type,
                         pattern = "outcomes|risk|mean|median|cases")) {
    data <- data %>%
      dplyr::select(.data$.exposure, outcome = dplyr::one_of(outcome)) %>%
      dplyr::mutate(event = NA, time = NA)
  } else {
    if(stringr::str_detect(string = type, pattern = "events|rate") |
       type == "time") {
      # with 'time' and 'time2', the first is enter and the second is exit
      if(!is.na(time2)) {
        data <- data %>%
          dplyr::select(.data$.exposure,
                        event     = dplyr::one_of(event),
                        time      = dplyr::one_of(time),
                        time2     = dplyr::one_of(time2)) %>%
          dplyr::mutate(time = time2 - time, outcome = NA)
      } else {
        data <- data %>% dplyr::select(.data$.exposure,
                                       event     = dplyr::one_of(event),
                                       time      = dplyr::one_of(time)) %>%
          dplyr::mutate(outcome = NA)
      }
    } else {
      data <- data %>%
        dplyr::select(.data$.exposure) %>%
        dplyr::mutate(event = NA, time = NA, outcome = NA)
    }
  }
  if(is.null(to))
    to <- dplyr::if_else(stringr::str_detect(string = type,
                                             pattern = "mean|median"),
                         true = " to ", false = "-")

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
      type == "cases/controls"        ~ paste(sum(.data$outcome),
                                              sum(!.data$outcome),
                                              sep = "/"),
      type == "risk"                  ~
        paste0(trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = "")),
      type == "risk (ci)"             ~
        paste0(trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), " (",
               trimws(format(round(scoreci(success = sum(.data$outcome),
                                           total = n())$conf.low *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)), to,
               trimws(format(round(scoreci(success = sum(.data$outcome),
                                           total = n())$conf.high *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)), ")"),
      type == "rate"                  ~
        trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                            digits = digits), nsmall = digits)),
      type == "rate (ci)"             ~
        paste0(trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                                   digits = digits), nsmall = digits)), " (",
               trimws(format(round(factor * exp(log(sum(.data$event) /
                                                      sum(.data$time))
                                                - stats::qnorm(0.975) *
                                                  1/sqrt(sum(.data$event))),
                                   digits = digits), nsmall = digits)),
               to,
               trimws(format(round(factor * exp(log(sum(.data$event) /
                                                      sum(.data$time))
                                                + stats::qnorm(0.975) *
                                                  1/sqrt(sum(.data$event))),
                                   digits = digits), nsmall = digits)), ")"),
      type == "outcomes (risk)" ~
        paste0(sum(.data$outcome), " (",
               trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), ")"),
      type == "outcomes/total (risk)" ~
        paste0(sum(.data$outcome), "/", n(), " (",
               trimws(format(round(sum(.data$outcome) / n() *
                                     if_else(risk_percent == TRUE,
                                             true = 100, false = 1),
                                   digits = digits), nsmall = digits)),
               if_else(risk_percent == TRUE, true = "%", false = ""), ")"),
      type == "events/time (rate)"    ~
        paste0(sum(.data$event), "/",
               trimws(format(round(sum(.data$time), digits = 0), nsmall = 0)),
               " (",
               trimws(format(round(sum(.data$event) * factor / sum(.data$time),
                                   digits = digits), nsmall = digits)), ")"),
      type == "mean" ~
        trimws(format(round(mean(.data$outcome), digits = digits),
                      nsmall = digits)),
      type == "mean (ci)" ~
        paste0(trimws(format(round(mean(.data$outcome), digits = digits),
                             nsmall = digits)), " (",
               trimws(format(round(mean(.data$outcome) - stats::qnorm(0.975) *
                                     sqrt(var(.data$outcome) /
                                            sum(!is.na(.data$outcome))),
                                   digits = digits),
                             nsmall = digits)),
               to,
               trimws(format(round(mean(.data$outcome) + stats::qnorm(0.975) *
                                     sqrt(var(.data$outcome) /
                                            sum(!is.na(.data$outcome))),
                                   digits = digits),
                             nsmall = digits)), ")"),
      type == "median" ~
        trimws(format(round(median(.data$outcome), digits = digits),
                      nsmall = digits)),
      type == "median (iqr)" ~
        paste0(trimws(format(round(median(.data$outcome, na.rm = TRUE),
                                   digits = digits), nsmall = digits)),
               " (",
               trimws(format(round(stats::quantile(.data$outcome,
                                                   probs = 0.25, na.rm = TRUE),
                                   digits = digits), nsmall = digits)),
               to,
               trimws(format(round(stats::quantile(.data$outcome,
                                                   probs = 0.75, na.rm = TRUE),
                                   digits = digits), nsmall = digits)), ")")),
      .groups = "drop")
}

#' Get point estimate and CI from regression models
#'
#' @param data Dataset
#' @param event Event variable
#' @param time Time variable
#' @param time2 Second time variable
#' @param outcome Outcome variable
#' @param effectmodifier Effect modifier variable
#' @param effectmodifier_level Effect modifier level
#' @param confounders String of covariates
#' @param risk_percent Display risk differences as percentage?
#' @param digits Number of digits to round estimates to
#' @param to Separator character(s) for confidence interval bounds
#' @param is_trend If called on a continous (trend) variable
#'
#' @return Tibble
#' @noRd
table_regress <- function(data, estimand, event, time, time2, outcome,
                          effectmodifier = NULL, effectmodifier_level = NULL,
                          confounders = "", risk_percent = FALSE, digits = 2,
                          is_trend = FALSE,
                          to = "-") {
  xlevels <- data %>% dplyr::pull(.data$.exposure) %>% levels()

  if(stringr::str_detect(string = estimand, pattern = "_joint") &
     is_trend == FALSE) {  # trends must be stratum-specific
    if(missing(effectmodifier) | missing(effectmodifier_level))
      stop(paste0("Effect modifier and stratum must be specified for joint model ('",
                  estimand, "')."))
    if(is.na(effectmodifier) | is.null(effectmodifier) |
       is.null(effectmodifier_level))
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
      if(!is.null(effectmodifier_level) & !(is.null(effectmodifier) |
                                            is.na(effectmodifier))) {
        data <- data %>%
          dplyr::rename(.effectmod = dplyr::one_of(effectmodifier)) %>%
          dplyr::filter(.data$.effectmod %in% effectmodifier_level)
      }
    }
  }

  # Select specific quantiles for quantreg, if provided:
  if(stringr::str_detect(string = estimand, pattern = "quantreg")) {
    tau <- stringr::str_remove_all(string = estimand,
                                   pattern = "quantreg|_joint|\\h")
    if(stringr::str_length(string = tau) > 0)
      tau <- as.numeric(tau)
    else
      tau <- 0.5
    if(is.na(tau))
      stop(paste0("The supplied quantile is not a valid numeric value: '",
                  estimand, "'."))
    if(stringr::str_detect(string = estimand, pattern = "_joint"))
      estimand <- "quantreg_joint"
    else
      estimand <- "quantreg"
  }

  if(stringr::str_detect(string = estimand, pattern = "^rd|^rr")) {
    # Select bootstrap repeats, if provided:
    bootrepeats <- as.numeric(stringr::str_extract(string = estimand,
                                                   pattern = "[:digit:]+"))
    if(!is.na(bootrepeats))
      bootrepeats <- max(bootrepeats, 50)
    # Select model fitting approach for riskdiff or riskratio, if provided:
    approach <- stringr::str_remove_all(string = estimand,
                                        pattern = "rd|rr|_joint|\\h|[:digit:]")
    if(stringr::str_length(string = approach) == 0)
      approach <- "auto"
    if(stringr::str_detect(string = estimand, pattern = "rd")) {
      possible_approaches <- as.character(as.list(args(risks::riskdiff))$approach)
    } else {
      possible_approaches <- as.character(as.list(args(risks::riskratio))$approach)
    }
    possible_approaches <- possible_approaches[!(possible_approaches %in%
                                                   c("c", "all"))]
    if(!(approach %in% possible_approaches)) {
      warning(paste0("Fitting a risks model via approach '",
                     approach,
                     "' is not implemented. Possible approaches are: ",
                     paste(possible_approaches, sep = ", ", collapse = ", "),
                     ". Defaulting to 'auto'."))
      approach <- "auto"
    }
    if(stringr::str_detect(string = estimand, pattern = "_joint")) {
      estimand <- paste0(stringr::str_extract(string = estimand,
                                              pattern = "rd|rr"), "_joint")
    } else {
      estimand <- stringr::str_extract(string = estimand, pattern = "rd|rr")
    }
  }
  if(exists("bootrepeats")) {
    if(is.na(bootrepeats))
      bootrepeats <- 1000  # default if not provided. only used for risks models
  } else {
    bootrepeats <- 1000
  }

  if(is.na(confounders))
    confounders <- ""

  multiply <- 1
  fit <- switch(EXPR = estimand,
                hr_joint =,
                hr = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  if(is.na(time2))
                    survival::coxph(formula = stats::as.formula(
                      paste0("survival::Surv(time = ", time, ",
                             event = ", event, ") ~ .exposure", confounders)),
                      data = data)
                  else
                    survival::coxph(formula = stats::as.formula(
                      paste0("survival::Surv(time = ", time, ", time2 =", time2,
                             ", event = ", event, ") ~ .exposure", confounders)),
                      data = data)
                },
                rr_joint =,
                rr = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  risks::riskratio(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    data = data, approach = approach)
                },
                rd_joint =,
                rd = {
                  reference <- 0
                  exponent <- FALSE
                  multiply <- if_else(risk_percent == TRUE,
                                      true = 100, false = 1)
                  to <- dplyr::if_else(is.null(to), true = " to ", false = to)
                  risks::riskdiff(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    data = data, approach = approach)
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
                foldlog_joint =,
                foldlog = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  stats::lm(formula = stats::as.formula(
                    paste0("log(", outcome, ") ~ .exposure ", confounders)),
                    data = data)
                },
                or_joint =,
                or = {
                  reference <- 1
                  exponent <- TRUE
                  to <- dplyr::if_else(is.null(to), true = "-", false = to)
                  stats::glm(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    family = stats::binomial(link = "logit"),
                    data = data)
                },
                quantreg_joint =,
                quantreg = {
                  reference <- 0
                  exponent <- FALSE
                  to <- dplyr::if_else(is.null(to), true = " to ", false = to)
                  quantreg::rq(formula = stats::as.formula(
                    paste(outcome, "~ .exposure", confounders)),
                    tau = tau, method = "fn",
                    data = data)
                },
                stop(paste0("Estimand '", estimand, "' is not implemented.")))

  fit <- switch(EXPR = estimand,
                # tidy.rq does not like "exponentiate" argument:
                quantreg =,
                quantreg_joint = broom::tidy(fit, conf.int = TRUE),
                # tidy.lm ignores "exponentiate":
                foldlog =,
                foldlog_joint = {
                  broom::tidy(fit, conf.int = TRUE) %>%
                    dplyr::mutate_at(.vars = dplyr::vars(.data$estimate,
                                                         .data$conf.low,
                                                         .data$conf.high),
                                     .funs = exp) },
                # otherwise:
                broom::tidy(fit, conf.int = TRUE,
                            exponentiate = exponent,
                            # only used for rr or rd with margstd:
                            bootrepeats = bootrepeats))

  fit <- fit %>%
    dplyr::select(.data$term, .data$estimate,
                  .data$conf.low, .data$conf.high) %>%
    dplyr::mutate_if(.predicate = is.numeric,
                     .funs = ~format(round(. * multiply, digits = digits),
                                     nsmall = digits,
                                     trim = TRUE, scientific = FALSE)) %>%
    dplyr::filter(stringr::str_detect(string = .data$term,
                                      pattern = pattern)) %>%
    dplyr::mutate(.exposure = stringr::str_remove(string = .data$term,
                                                  pattern = pattern))
  if(is_trend == TRUE) {
    fit <- fit %>%
      dplyr::slice(1) %>%
      dplyr::mutate(.exposure = "Trend")
  } else {
    fit <- fit %>%
      dplyr::left_join(x = tibble::tibble(.exposure = xlevels),
                       by = ".exposure")
  }
  fit %>%
    dplyr::mutate(res = paste0(.data$estimate, " (", .data$conf.low, to,
                               .data$conf.high, ")"),
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
#' @param time2 Second time variable
#' @param exposure Exposure variable
#' @param effectmodifier Effect modifier variable
#' @param stratum Effect modifier level
#' @param confounders String of covariates
#' @param type Type of statistic quested from table_count
#' @param trend Continuous (trend) exposure variable
#' @param factor Factor for rates. Defaults to 1000.
#' @param risk_percent Show risks and risk differences as percentages?
#' @param diff_digits Number of digits to round difference estimates to
#' @param ratio_digits Number of digits to round ratio estimates to
#' @param rate_digits Number of digits to round rate estimates to
#' @param to Separator for mean/difference confidence intervals.
#'
#' @return Tibble
#' @noRd
fill_cells <- function(data, event, time, time2, outcome,
                       exposure, effect_modifier, stratum, confounders,
                       type, trend, factor, risk_percent,
                       diff_digits, ratio_digits, rate_digits, to) {
  data <- data %>% dplyr::rename(.exposure = dplyr::one_of(exposure))

  # Check that exposure is categorical
  if(!(class(data %>% pull(.data$.exposure))[1] %in% c("factor", "character",
                                                       "logical")))
    warning(paste0("Exposure variable '", exposure,
                   "' is not categorical (factor, character, or logical). ",
                   "Its type was changed to 'factor' but the result may be ",
                   "undesirable, e.g., if the variable is actually continuous ",
                   "and thus has many levels."))
  data$.exposure <- factor(data$.exposure)

  # Check that trend variable, if given, is continuous
  if(!is.na(trend)) {
    if(!(trend %in% names(data)))
      stop(paste0("Trend variable '", trend, "' is not valid for the dataset."))
    data <- data %>% dplyr::rename(.trend = dplyr::one_of(trend))
    if(class(data %>% pull(.data$.trend))[1] != "numeric")
      stop(paste0("Trend variable '", trend,
                     "' is not continuous (numeric)."))
  }

  if(type == "" | type == "blank")
    return(tibble::tibble(.exposure = data %>% dplyr::pull(.data$.exposure) %>%
                            levels(),
                          res = ""))

  # Check that time and event variable exist, if needed
  if(stringr::str_detect(string = type, pattern = "events|hr|rate|time")) {
    if(!(event %in% names(data)))
      stop(paste0("Survival data using type = '", type, "' requested, but ",
                  "event variable '", event, "' is not valid for the dataset."))
    if(!(time %in% names(data)))
      stop(paste0("Survival data using type = '", type, "' requested, but ",
                  "time variable '", time, "' is not valid for the dataset."))
    if(!is.na(time2))
      if(!(time2 %in% names(data)))
        stop(paste0("Survival data using type = '", type, "' with enter and ",
                    "exit times was requested, but the second time variable '",
                    time2, "' is not valid for the dataset."))
  }

  # Check that outcome variable exists, if needed
  if(stringr::str_detect(
    string = type,
    pattern = "outcomes|diff|mean|median|risk|^rr|rd|irr|fold|foldlog|or|cases|quantreg")) {
    if(!(outcome %in% names(data)))
      stop(paste0("Using type = '", type, "' requires an outcome variable, ",
                  "but the variable '", outcome,
                  "' is not valid for the dataset."))
    # Check that outcome is binary
    outcomevar <- data %>%
      dplyr::select(outcome = dplyr::one_of(outcome)) %>%
      dplyr::pull(outcome)
    if(!(stringr::str_detect(
      string = type,
      pattern = "diff|mean|median|irr|fold|foldlog|quantreg")))
      if(!(all(sort(unique(outcomevar)) == c(0, 1)) |
           all(sort(unique(outcomevar)) == c(FALSE, TRUE))))
        stop(paste0("Outcome variable '", outcome,
                    "' must be binary with levels c(0, 1) or c(FALSE, TRUE)."))
  }

  # extract desired digits from individual "type" fields
  if(stringr::str_detect(string = type,
                         pattern = "\\(ci\\)|\\(iqr\\)|\\(risk\\)|\\(rate\\)")) {
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
    digits <- dplyr::case_when(
      stringr::str_detect(string = type,
                          pattern = "hr|rr|irr|fold|foldlog|or") ~
        ratio_digits[1],
      stringr::str_detect(string = type,
                          pattern = "rd|diff|mean|risk|outcomes|median|quantreg") ~
        diff_digits[1],
      stringr::str_detect(string = type,
                          pattern = "rate") ~
        rate_digits[1],
      TRUE ~ 4)
  }

  if(stringr::str_detect(string = type,
                         pattern = "hr|^rr|rd|irr|fold|foldlog|diff|or|quantreg")) {
    res_cat <- table_regress(data = data,
                             estimand = type,
                             event = event,
                             time = time,
                             time2 = time2,
                             outcome = outcome,
                             effectmodifier = effect_modifier,
                             effectmodifier_level = stratum,
                             confounders = confounders,
                             digits = digits,
                             risk_percent = risk_percent,
                             to = to)
    if(!is.na(trend)) {
      dplyr::bind_rows(res_cat,
                       table_regress(data = data %>%
                                       dplyr::mutate(.exposure = .data$.trend),
                                     estimand = type,
                                     event = event,
                                     time = time,
                                     time2 = time2,
                                     outcome = outcome,
                                     effectmodifier = effect_modifier,
                                     effectmodifier_level = stratum,
                                     confounders = confounders,
                                     digits = digits,
                                     risk_percent = risk_percent,
                                     to = to,
                                     is_trend = TRUE))
    } else {
      res_cat
    }
  } else {
    table_counts(data = data,
                 event = event,
                 time = time,
                 time2 = time2,
                 outcome = outcome,
                 effectmodifier = effect_modifier,
                 effectmodifier_level = stratum,
                 type = type,
                 factor = factor,
                 digits = digits,
                 risk_percent = risk_percent,
                 to = to)
  }
}

#' Table 2: Stratified Result Tables
#'
#' @description This function displays descriptive
#' and inferential results for binary, continuous, and survival data
#' in the format of a table stratified by exposure and, if requested, by
#' effect modifiers.
#'
#' This function is intended only for tabulations of final results.
#' Model diagnostics for regression models need to be conducted separately.
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
#'   rounding of means, risks, and risk difference estimates. Defaults to
#'   \code{2} for \code{risk_percent = FALSE} and to \code{0} for
#'   \code{risk_percent = TRUE}. Can override for each line in \code{type}.
#' @param ratio_digits Optional. Number of decimal digits to show for ratio
#'   estimates. Defaults to \code{2}. Can override for each line in \code{type}.
#' @param rate_digits Optional. Number of decimal digits to show for rates.
#'   Defaults to \code{1}. Can override for each line in \code{type}.
#' @param to Optional. Separator between the lower and the upper bound
#'   of the 95% confidence interval (and interquartile range for medians).
#'   Defaults to \code{" to "} for means, medians, and differences in means or
#'   quantiles; defaults to \code{"-"} otherwise.
#' @param type2_layout Optional. If a second estimate is requested via
#'   \code{type2} in the \code{design} matrix, display it as in rows below
#'   (\code{"rows"}) or columns (\code{"columns"}) to the right. Defaults to
#'   \code{"rows"}.
#' @param prepare_md Optional. Defaults to \code{TRUE}. Prepare labels for
#'   printing with markdown (\code{%>% gt:gt() %>%
#'   gt::fmt_markdown(columns = 1)}).
#'   This step will replace two spaces or a tab at the beginning of a
#'   \code{label} with HTML code for an indentation (\code{&ensp;}).
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
#'   *  \code{time2} The second time variable for late entry models.
#'        Only used in conjunction with \code{time}. If provided,
#'        \code{time} will become the entry time and \code{time2}
#'        the exit time, following conventions of
#'        \code{\link[survival]{Surv}}.
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
#'   *  \code{trend} Optional. For regression models, a continuous
#'        representation of the exposure, for which a slope per one unit
#'        increase ("trend") will be estimated. Must be a numeric variable.
#'        If joint models for \code{exposure} and \code{effect_modifier} are
#'        requested, trends are still reported within each stratum of the
#'        \code{effect_modifier}. Use \code{NA} to leave blank.
#'   *  \code{effect_modifier} Optional. A categorical effect modifier variable.
#'        Use \code{NA} to leave blank.
#'   *  \code{stratum} Optional. A stratum of the effect modifier.
#'        Use \code{NULL} to leave blank. \code{NA} will evaluate
#'        observations with missing data for the \code{effect_modifier}.
#'   *  \code{confounders} Optional. A string in the format
#'        \code{"+ var1 + var2"} that will be substituted into
#'        into \code{formula = exposure + confounders}.
#'        Use \code{NA} or \code{""} (empty string) to leave blank; the default.
#'        For Cox models, can add \code{"+ strata(site)"}
#'        to obtain models with stratification by, e.g., \code{site}.
#'        For Poisson models, can add \code{"+ offset(log(persontime))"}
#'        to define, e.g., \code{persontime} as the offset.
#'   *  \code{type} The statistic requested (case-insensitive):
#'
#'      Comparative estimates from regression models
#'      with 95% confidence intervals:
#'
#'      * \code{"hr"} Hazard ratio from Cox proportional
#'        hazards regression.
#'      * \code{"irr"} Incidence rate ratio for count outcomes
#'        from Poisson regression model.
#'      * \code{"rr"} Risk ratio (or prevalence ratio)
#'        from \code{\link[risks]{riskratio}}. Can request specific model
#'        fitting  approach and, for marginal
#'        standardization only, the number of bootstrap repeats.
#'        Examples: \code{"rrglm_start"} or \code{"rrmargstd2000"}.
#'        (Note absence of white space.)
#'      * \code{"rd"} Risk difference (or prevalence difference)
#'        from \code{\link[risks]{riskdiff}}. Can request model fitting approach
#'        and bootstrap repeats as for \code{"rr"}.
#'      * \code{"diff"} Mean difference from linear model.
#'      * \code{"quantreg"} Quantile difference from quantile regression using
#'        \code{\link[quantreg]{rq}} with \code{method = "fn"}.
#'        By default, this is the difference in medians. For a different
#'        quantile, e.g., the 75th percentile, use \code{"quantreg0.75"}.
#'        Note absence of white space before the quantile.
#'      * \code{"fold"} Fold change from generalized linear
#'        model with log link (i.e., ratio of arithmetic means).
#'      * \code{"foldlog"} Fold change from linear
#'        model after log transformation of the outcome
#'        (i.e., ratio of geometric means).
#'      * \code{"or"} Odds ratio from logistic regression.
#'
#'      Absolute estimates per exposure category:
#'
#'      * \code{"events"} Event count.
#'      * \code{"time"} Person-time.
#'      * \code{"outcomes"} Outcome count.
#'      * \code{"total"} Number of observations.
#'      * \code{"events/time"} Events slash person-time.
#'      * \code{"events/total"} Events slash number of observations.
#'      * \code{"cases/controls"} Cases and non-cases;
#'        for case-control studies.
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
#'      \code{"fold"}, \code{"foldlog"}, \code{"quantreg"}, or \code{"or"} to
#'      obtain "joint" models for exposure and effect modifier that have a
#'      single reference category.
#'      Example: \code{type = "hr_joint"}. The reference categories
#'      for exposure and effect modifier are their first factor levels, which
#'      can be changed using \code{\link[forcats]{fct_relevel}}.
#'
#'      Digits for rounding estimates can be specified for each line separately.
#'      Example: \code{type = "diff (ci) 3"} to request a mean difference
#'      and its 95% CI rounded to 3 decimal digits (note the space before
#'      \code{3}).
#'
#' Hint: Use \code{\link[tibble]{tibble}}, \code{\link[tibble]{tribble}}, and
#'   \code{\link[dplyr]{mutate}} to construct the \code{design} dataset,
#'   especially variables that are used repeatedly (e.g., \code{exposure, time,
#'   event}, or \code{outcome}). See examples.
#'
#' @return Tibble. Get formatted output by passing on to
#'   \code{\link[khsmisc]{mygt}}.
#' @export
#'
#' @examples
#' # Load 'cancer' dataset from survival package
#' data(cancer, package = "survival")
#'
#' # The exposure (here, 'sex') must be categorical
#' cancer <- cancer %>%
#'   tibble::as_tibble() %>%
#'   dplyr::mutate(sex = factor(sex, levels = 1:2,
#'                              labels = c("Men", "Women")),
#'                 time = time / 365.25,
#'                 status = status - 1)
#'
#' # Example 1: Binary outcomes (use 'outcome' variable)
#' # Set table design
#' design1 <- tibble::tibble(
#'   label = c("Outcomes",
#'             "Total",
#'             "Outcomes/Total",
#'             "Risk",
#'             "Risk (CI)",
#'             "Outcomes (Risk)",
#'             "Outcomes/Total (Risk)",
#'             "RR",
#'             "RD")) %>%
#'   dplyr::mutate(type = label,
#'                 exposure = "sex",
#'                 outcome = "status")
#'
#' # Generate table2
#' table2(design = design1, data = cancer)
#'
#' # Use 'design' as columns (selecting RR and RD only)
#' table2(design = design1 %>%
#'                   dplyr::filter(label %in% c("RR", "RD")),
#'        data = cancer,
#'        layout = "cols")
#'
#' # Example 2: Survival outcomes (use 'time' and 'event'),
#' #   with an effect modifier and a confounder
#' # Set table design
#' design2 <- tibble::tribble(
#'   # Elements that vary by row:
#'   ~label,                       ~stratum, ~confounders, ~type,
#'   "**Overall: Events**",        NULL,     "",           "events",
#'   "  Person-years",             NULL,     "",           "time",
#'   "  Rate/1000 py (95% CI)",    NULL,     "",           "rate (ci)",
#'   "  Unadjusted HR (95% CI)",   NULL,     "",           "hr",
#'   "  Age-adjusted HR (95% CI)", NULL,     "+ age",      "hr",
#'   "",                           NULL,     "",           "blank",
#'   "**Stratified models**",      NULL,     "",           "blank",
#'   "*ECOG PS1* (events/N)",      1,        "",           "events/total",
#'   "  Unadjusted",               1,        "",           "hr",
#'   "  Age-adjusted",             1,        "+ age",      "hr",
#'   "*ECOG PS2* (events/N)",      2,        "",           "events/total",
#'   "  Unadjusted",               2,        "",           "hr",
#'   "  Age-adjusted",             2,        "+ age",      "hr",
#'   "",                           NULL,     "",           "",
#'   "**Joint model**, age-adj.",  NULL,     "",           "",
#'   "  ECOG PS1",                 1,        "+ age",      "hr_joint",
#'   "  ECOG PS2",                 2,        "+ age",      "hr_joint") %>%
#'   # Elements that are the same for all rows:
#'   dplyr::mutate(exposure = "sex",
#'                 event = "status",
#'                 time = "time",
#'                 effect_modifier = "ph.ecog")
#'
#' # Generate table2
#' table2(design = design2,
#'        data = cancer %>% dplyr::filter(ph.ecog %in% 1:2))
#'
#' # Example 3: Get two estimates using 'type' and 'type2'
#' design3 <- tibble::tribble(
#'   ~label,     ~stratum, ~type,          ~type2,
#'   "ECOG PS1", 1,        "events/total", "hr",
#'   "ECOG PS2", 2,        "events/total", "hr") %>%
#'   dplyr::mutate(exposure = "sex",
#'                 event = "status",
#'                 time = "time",
#'                 confounders = "+ age",
#'                 effect_modifier = "ph.ecog")
#'
#' table2(design = design3,
#'        data = cancer %>% dplyr::filter(ph.ecog %in% 1:2))
#'
#' table2(design = design3,
#'        data = cancer %>% dplyr::filter(ph.ecog %in% 1:2),
#'        layout = "cols", type2_layout = "cols")
#'
#'
#' # Example 4: Continuous outcomes (use 'outcome' variable);
#' # request rounding to 1 decimal digit in some cases;
#' # add continuous trend (slope per one unit of the 'trend' variable)
#' tibble::tribble(
#'   ~label,                   ~stratum, ~type,
#'   "Marginal mean (95% CI)", NULL,     "mean (ci) 1",
#'   "  Men",                  "Men",    "mean",
#'   "  Women",                "Women",  "mean",
#'   "",                       NULL,     "",
#'   "Stratified model",       NULL,     "",
#'   "  Men",                  "Men",    "diff 1",
#'   "  Women",                "Women",  "diff 1",
#'   "",                       NULL,     "",
#'   "Joint model",            NULL,     "",
#'   "  Men",                  "Men",    "diff_joint",
#'   "  Women",                "Women",  "diff_joint") %>%
#'   dplyr::mutate(exposure = "ph.ecog_factor",
#'                 trend = "ph.ecog",
#'                 outcome = "age",
#'                 effect_modifier = "sex") %>%
#'   table2(data = cancer %>%
#'                   dplyr::filter(ph.ecog < 3) %>%
#'                   dplyr::mutate(ph.ecog_factor = factor(ph.ecog)))
#'
#' # Example 5: Get formatted output for Example 2 (see above)
#' \dontrun{
#' table2(design = design2,
#'        data = cancer %>% dplyr::filter(ph.ecog %in% 1:2)) %>%
#'   mygt(md = 1)  # get markdown formatting in first column ('label')
#' }
table2 <- function(design, data, layout = "rows", factor = 1000,
                   risk_percent = FALSE,
                   diff_digits = dplyr::if_else(risk_percent == TRUE,
                                                true = 0, false = 2),
                   ratio_digits = 2, rate_digits = 1, to = NULL,
                   type2_layout = "rows",
                   prepare_md = TRUE) {
  if(!is.data.frame(design))
    stop("No 'design' data frame/tibble was provided.")
  name <- labelled::var_label(dplyr::pull(data, design$exposure[1]))
  if(is.null(name))
    name <- design$exposure[1]

  if(!("event"       %in% names(design))) design$event       <- NA
  if(!("time"        %in% names(design))) design$time        <- NA
  if(!("time2"       %in% names(design))) design$time2       <- NA
  if(!("outcome"     %in% names(design))) design$outcome     <- NA
  if(!("trend"       %in% names(design))) design$trend       <- NA
  if(!("confounders" %in% names(design))) design$confounders <- ""
  if(!("type2"       %in% names(design))) design$type2       <- ""
  if(!("effect_modifier" %in% names(design) & "stratum" %in% names(design))) {
    design <- design %>%
      dplyr::mutate(effect_modifier = NA, stratum = NA)
  }
  design <- design %>%
    dplyr::mutate(type2 = dplyr::if_else(is.na(.data$type2),
                                         true = "", false = .data$type2))

  if(prepare_md == TRUE) {
    design <- design %>%
      dplyr::mutate(label = stringr::str_replace_all(string = .data$label,
                                                     pattern = "^\\h\\h",
                                                     replacement = "&ensp;"),
                    label = stringr::str_replace_all(string = .data$label,
                                                     pattern = "^&ensp;\\h\\h",
                                                     replacement = "&ensp;&ensp;"))
  }

  res <- design %>%
    dplyr::mutate(type   = stringr::str_to_lower(string = .data$type),
                  type2  = stringr::str_to_lower(string = .data$type2),
                  index  = dplyr::row_number(),
                  result = purrr::pmap(.l = list(.data$event,
                                                 .data$time, .data$time2,
                                                 .data$outcome,
                                                 .data$exposure,
                                                 .data$effect_modifier,
                                                 .data$stratum,
                                                 .data$confounders,
                                                 .data$type,
                                                 .data$trend),
                                       .f = fill_cells,
                                       data = data,
                                       factor = factor,
                                       risk_percent = risk_percent,
                                       diff_digits = diff_digits,
                                       ratio_digits = ratio_digits,
                                       rate_digits = rate_digits,
                                       to = to))

  # simple reshaping if only "type" alone
  if(all(design$type2 == "")) {
    res <- res %>%
      dplyr::select(.data$index, .data$label, .data$result) %>%
      tidyr::unnest(cols = .data$result)
    if(layout == "rows") {
      res %>%
        tidyr::pivot_wider(names_from = .data$.exposure,
                           values_from = .data$res,
                           values_fill = "") %>%
        dplyr::rename(!!name := .data$label) %>%
        dplyr::select(-.data$index)
    } else {
      if(sum(duplicated(design$label)) > 0 | "" %in% design$label) {
        res %>%
          tidyr::pivot_wider(names_from = c(.data$index, .data$label),
                             values_from = .data$res,
                             values_fill = "")
      } else {
        res %>%
          dplyr::select(-.data$index) %>%
          tidyr::pivot_wider(names_from = .data$label,
                             values_from = .data$res,
                             values_fill = "") %>%
          dplyr::rename(!!name := .data$.exposure)
      }
    }

  # handle "type" and "type2" together
  } else {
    res <- res %>%
      dplyr::mutate(result2 = purrr::pmap(.l = list(.data$event,
                                                    .data$time, .data$time2,
                                                    .data$outcome,
                                                    .data$exposure,
                                                    .data$effect_modifier,
                                                    .data$stratum,
                                                    .data$confounders,
                                                    .data$type2,  # !
                                                    .data$trend),
                                          .f = fill_cells,
                                          data = data,
                                          factor = factor,
                                          risk_percent = risk_percent,
                                          diff_digits = diff_digits,
                                          ratio_digits = ratio_digits,
                                          rate_digits = rate_digits,
                                          to = to)) %>%
      dplyr::mutate(result = purrr::map2(.x = .data$result,
                                         .y = .data$result2,
                                         .f = ~full_join(.x, .y,
                                                         by = ".exposure",
                                                         suffix = c(".1",
                                                                    ".2")))) %>%
      dplyr::select(.data$index, .data$label, .data$result) %>%
      tidyr::unnest(cols = .data$result) %>%
      tidyr::pivot_longer(cols = c(.data$res.1, .data$res.2),
                          names_to = "whichres", values_to = "value") %>%
      dplyr::mutate(value = dplyr::if_else(is.na(.data$value),
                                           true = "", false = .data$value))
    if(layout == "rows") {
      if(type2_layout == "rows") {
        res %>%
          tidyr::pivot_wider(names_from = .data$.exposure,
                             values_from = .data$value,
                             values_fill = "") %>%
          dplyr::group_by(.data$index) %>%
          dplyr::mutate(label = dplyr::if_else(dplyr::row_number() == 1,
                                               true = .data$label,
                                               false = "")) %>%
          dplyr::ungroup() %>%
          dplyr::rename(!!name := .data$label) %>%
          dplyr::select(-.data$index, -.data$whichres)
      } else {
        res %>%
          dplyr::mutate(.exposure = dplyr::if_else(.data$whichres == "res.1",
                                                   true = .data$.exposure,
                                                   false = paste0(.data$.exposure,
                                                                  " "))) %>%
          dplyr::select(-.data$whichres) %>%
          tidyr::pivot_wider(names_from = .data$.exposure,
                             values_from = .data$value,
                             values_fill = "") %>%
          dplyr::rename(!!name := .data$label) %>%
          dplyr::select(-.data$index)
      }
    } else {
      if(type2_layout == "rows") {
        if(sum(duplicated(design$label)) > 0 | "" %in% design$label) {
          res <- res %>%
            tidyr::pivot_wider(names_from = c(.data$index, .data$label),
                               values_from = .data$value,
                               values_fill = "")
        } else {
          res <- res %>%
            dplyr::select(-.data$index) %>%
            tidyr::pivot_wider(names_from = .data$label,
                               values_from = .data$value,
                               values_fill = "")
        }
        res %>%
          dplyr::group_by(.data$.exposure) %>%
          dplyr::mutate(.exposure = if_else(.data$whichres == "res.1",
                                            true = .data$.exposure,
                                            false = "")) %>%
          dplyr::ungroup() %>%
          dplyr::select(-.data$whichres) %>%
          dplyr::rename(!!name := .data$.exposure)
      } else {
        if(sum(duplicated(design$label)) > 0 | "" %in% design$label) {
          res %>%
            dplyr::mutate(label = dplyr::if_else(.data$whichres == "res.1",
                                                 true = .data$label,
                                                 false = paste0(.data$label,
                                                                " "))) %>%
            dplyr::select(-.data$whichres) %>%
            tidyr::pivot_wider(names_from = c(.data$index, .data$label),
                               values_from = .data$value,
                               values_fill = "") %>%
            dplyr::rename(!!name := .data$.exposure)
        } else {
          res %>%
            dplyr::mutate(label = dplyr::if_else(.data$whichres == "res.1",
                                                 true = .data$label,
                                                 false = paste0(.data$label,
                                                                " "))) %>%
            dplyr::select(-.data$whichres, -.data$index) %>%
            tidyr::pivot_wider(names_from = .data$label,
                               values_from = .data$value,
                               values_fill = "") %>%
            dplyr::rename(!!name := .data$.exposure)
        }
      }
    }
  }
}
