#' Estimate Restricted Mean Time Lost (RMTL) and its Difference
#'
#' The method of Connor and Trinquart (Stat Med 2021) estimates the RMTL in the
#' presence of competing risks. This function a modification of their R code.
#' Estimation functions are identical; input and output handling have been
#' adapted.
#'
#' @param data Data frame (tibble).
#' @param exposure Optional. Name of exposure variable within the \code{data}.
#'   If omitted, will return unstratified RMTL.
#' @param time Name of time variable within the \code{data}. By default, this
#'   is the event time. If \code{time2} is also given, this is the entry time,
#'   as per \code{\link[survival]{Surv}} standard.
#' @param time2 Optional. Name of the variable  within the \code{data}
#'   indicating the exit (event) time. If omitted, \code{time} is  the event
#'   time, as per \code{\link[survival]{Surv}} standard.
#' @param event Name of event variable within the \code{data}. Can be integer,
#'   factor, character, or logical. The reference level indicates censoring and
#'   is taken as the first level of the factor, the lowest numeric value
#'   (usually \code{0}), or \code{FALSE} for a logical variables. Other levels
#'   are events of different types. If no competing risks, the alternative
#'   level, e.g. \code{1}, indicates the event (e.g., death).
#' @param event_of_interest Optional. Indicator for which of the non-censoring
#'   events is of main interest. Others are treated as competing. Defaults to
#'   \code{1}, i.e., the first non-censoring event.
#' @param weight Optional. Weights, such as inverse-probability weights or
#'   survey weights. The default (\code{NULL}) uses equal weights for all
#'   observations.
#' @param tau Optional. Time horizon to restrict event times. By default, the
#'   the latest time in the group with the shortest follow-up. Prefer a
#'   user-defined, interpretable time horizon.
#' @param conf.level Optional. Confidence level. Defaults to \code{0.95}.
#'
#' @details
#' Differences to the original function rmtl::rmtl():
#' * Convert print() to stop() for errors.
#' * Pass data as a data frame/tibble, select variables from it.
#' * Allow for different variable types in the \code{event} variable.
#' * Use \code{\link[survival]{Surv}} conventions for entry/exit times
#'   (\code{time}, \code{time2}).
#' * Return contrasts in restricted mean time lost as comparisons to the
#'   reference level instead of all pairwise comparisons.
#' * Add origin (time 0, cumulative incidence 0) to the returned cumulative
#'   incidence function for proper plotting.
#' * Return results as a list of tibbles. Do not print or plot.
#'
#' @return List:
#' * \code{tau} Time horizon.
#' * \code{rmtl} Tibble with absolute estimates of RMTL, per exposure group if
#'   given.
#' * \code{rmtdiff} Tibble with contrasts of RMTL between exposure groups,
#'   compared to a common reference (the first level). Empty tibble if no
#'   exposure variable given.
#' * \code{cif} Tibble with cumulative incidence function for the event of
#'   interest:
#'   * \code{exposure} Exposure group.
#'   * \code{time} Event time.
#'   * \code{estimate} Nelsen-Aalen estimate of cumulative incidence function
#'     with \code{se}, \code{conf.low}, and \code{conf.high}.
#'
#' @references Conner SC, Trinquart L. Estimation and modeling of the restricted
#' mean time lost in the presence of competing risks. Stat Med 2021;40:2177–96.
#' https://doi.org/10.1002/sim.8896.
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer <- cancer %>% dplyr::mutate(
#'   status = status - 1,   # make 0/1
#'   sex    = factor(sex, levels = 1:2, labels = c("Men", "Women")))
#'
#' result <- estimate_rmtl(
#'   data = cancer,
#'   exposure = sex,
#'   time = time,
#'   event = status,
#'   tau = 365.25)  # time horizon: one year
#' result
#'
#' # Make (simple) plot
#' library(ggplot2)
#' result$cif %>%
#'   ggplot(mapping = aes(x = time, y = estimate, color = exposure)) +
#'   geom_step() +
#'   scale_x_continuous(breaks = seq(from = 0, to = 365, by = 60)) +
#'   labs(x = "Time since start of follow-up in days", y = "Cumulative incidence")

estimate_rmtl <- function(data,
                          exposure = NULL,
                          time,
                          time2 = NULL,
                          event,
                          event_of_interest = 1,
                          weight = NULL,
                          tau = NULL,
                          conf.level = 0.95) {
  alldat <- data %>%
    tibble::as_tibble() %>%
    dplyr::select(time   = {{ time }},
                  time2  = {{ time2 }},
                  event  = {{ event }},
                  group  = {{ exposure }},
                  weight = {{ weight }})
  # set to default values if missing in data
  added_cols <- tibble(time2 = NA_real_, group = "Overall", weight = 1)
  alldat <- alldat %>%
    tibble::add_column(added_cols %>% select(-dplyr::any_of(names(alldat)))) %>%
    filter(!is.na(.data$group) & !is.na(.data$time)) %>%
    arrange(.data$group) %>%
    dplyr::mutate(
      group = factor(.data$group),
      entry = dplyr::if_else(is.na(.data$time2),
                             true = 0, false = .data$time),
      times = dplyr::if_else(is.na(.data$time2),
                             true = .data$time, false = .data$time2),
      event = as.numeric(factor(as.numeric(.data$event))) - 1) %>%
    dplyr::select(-.data$time, -.data$time2)

  if (any(alldat$times < 0)) {
    stop("Event/censoring times must be positive.")
  }

  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  gg <- length(levels(alldat$group))

  if (is.null(tau)) {
    tau <- alldat %>%
      dplyr::group_by(.data$group) %>%
      dplyr::summarize(maxfu = max(.data$times), .groups = "drop") %>%
      dplyr::summarize(tau = min(.data$maxfu)) %>%
      pull(.data$tau)
  } else {
    tau.error <- rep(0, gg)
    for (i in (1:gg)) {
      groupval <- (levels(alldat$group)[i])
      dat_group <- alldat[which(alldat$group == (groupval)), ]
      tau.error[i] <- ifelse(max(dat_group$times) < tau, 1, 0)
    }
    if (sum(tau.error) > 0) {
      stop(paste("Observed event/censoring times do not reach tau in each",
                 "exposure group. Choose a different tau or leave unspecified",
                 "for default value of tau."))
    }
  }

  alldat$event[alldat$times > tau] <- 0
  alldat$times[alldat$times > tau] <- tau
  rmtl <- rep(NA, length(1:gg))
  groupval <- rep(NA, length(1:gg))
  rmtl.se <- rep(NA, length(1:gg))
  res.cif <- list()

  for (g in 1:gg) {
    groupval[g] <- (levels(alldat$group)[g])
    data <- alldat[which(alldat$group == (groupval[g])), ]
    tj <- data$times[data$event != 0]
    tj <- unique(tj[order(tj)])
    num.tj <- length(tj)
    num.atrisk <-
      sapply(tj, function(x)
        sum(data$weight[data$entry < x & data$times >= x]))
    num.ev1 <-
      sapply(tj, function(x)
        sum(data$weight[data$event == event_of_interest &
                          data$times == x]))
    num.ev2 <-
      sapply(tj, function(x)
        sum(data$weight[data$event != event_of_interest &
                          data$event != 0 & data$times == x]))
    num.ev <- num.ev1 + num.ev2
    m <- sapply(tj, function(x) {
      sum((data$weight[data$entry < x & data$times >= x]) ^ 2)
    })
    mg <- ((num.atrisk ^ 2) / m)
    h1 <- num.ev1 / num.atrisk
    h <- num.ev / num.atrisk
    s <- cumprod(c(1, 1 - h))
    s <- s[1:length(s) - 1]
    theta <- s * h1
    cif1 <- cumsum(theta)
    a <- c(0, cumsum(num.ev / (mg * (num.atrisk - num.ev))))
    a <- a[1:num.tj]

    var.theta <- ((theta) ^ 2) *
      (((num.atrisk - num.ev1) / (mg * num.ev1)) + a)
    var.theta[is.nan(var.theta)] <- 0
    cov.theta <- matrix(NA, nrow = num.tj, ncol = num.tj)
    b <- c(0, cumsum(num.ev / (mg * (num.atrisk - num.ev))))
    for (j in 1:(num.tj - 1)) {
      for (k in (j + 1):num.tj) {
        cov.theta[k, j] <- cov.theta[j, k] <- (theta[j]) *
          (theta[k]) * (-1 / mg[j] + b[j])
      }
    }
    diag(cov.theta) <- var.theta
    cov.f10 <- apply(cov.theta, 2, function(x) {
      x[is.na(x)] <- 0
      cumsum(x)
    })
    cov.f1 <- apply(cov.f10, 1, function(x) {
      x[is.na(x)] <- 0
      cumsum(x)
    })
    var.f1 <- diag(cov.f1)
    areas <- c(tj[2:num.tj], tau) - tj
    rmtl[g] <- sum(areas * cif1)
    cov.weights <- outer(areas, areas)
    cov.f1.weight <- cov.weights * cov.f1
    rmtl.var <- sum(cov.f1.weight)
    rmtl.se[g] <- sqrt(rmtl.var)
    res.cif.g <- list()
    res.cif.g[[length(res.cif.g) + 1]] <- c(cif1, cif1[num.tj])
    res.cif.g[[length(res.cif.g) + 1]] <- c(sqrt(var.f1), sqrt(var.f1[num.tj]))
    res.cif.g[[length(res.cif.g) + 1]] <- c(tj, tau)
    res.cif.g[[length(res.cif.g) + 1]] <- c(
      cif1 - z * sqrt(var.f1),
      cif1[num.tj] - z * sqrt(var.f1[num.tj]))
    res.cif.g[[length(res.cif.g) + 1]] <- c(
      cif1 + z * sqrt(var.f1),
      cif1[num.tj] + z * sqrt(var.f1[num.tj]))
    names(res.cif.g) <- c("cif", "se.cif", "tj", "cif.cil", "cif.ciu")
    res.cif[[length(res.cif) + 1]] <- res.cif.g
  }

  # if(gg > 1) {  # moved from here
  res <- data.frame(
    groupval,
    rmtl,
    rmtl.se,
    cil = rmtl - (z * rmtl.se),
    ciu = rmtl + (z * rmtl.se))
  names(res.cif) <- groupval
  pwc <- gg  #((gg ^ 2) - gg) / 2
  # to here
  if (gg > 1) {
    if (pwc > 0) {
      label.diff <- rep(NA, pwc)
      rmtl.diff <- rep(NA, pwc)
      rmtl.diff.se <- rep(NA, pwc)
      rmtl.diff.cil <- rep(NA, pwc)
      rmtl.diff.ciu <- rep(NA, pwc)
      rmtl.diff.p <- rep(NA, pwc)
      res.diff <- data.frame(
        label.diff,
        rmtl.diff,
        rmtl.diff.se,
        rmtl.diff.cil,
        rmtl.diff.ciu,
        rmtl.diff.p)
      l <- 1
      i <- 1  # make contrasts only to reference level (1st group)

      for (ii in i:gg) { # (ii in (i + 1):gg) {
        res.diff[l, ]$label.diff <- res[ii, ]$groupval
        res.diff[l, ]$rmtl.diff <- (res[ii, ]$rmtl - res[i, ]$rmtl)
        res.diff[l, ]$rmtl.diff.se <- sqrt(res[ii, ]$rmtl.se ^ 2 +
                                             res[i, ]$rmtl.se ^2)
        res.diff[l, ]$rmtl.diff.cil <- res.diff[l, ]$rmtl.diff -
          z * res.diff[l, ]$rmtl.diff.se
        res.diff[l, ]$rmtl.diff.ciu <- res.diff[l, ]$rmtl.diff +
          z * res.diff[l, ]$rmtl.diff.se
        res.diff[l, ]$rmtl.diff.p <- 2 * (1 - stats::pnorm(
          abs(res.diff[l, ]$rmtl.diff) / res.diff[l, ]$rmtl.diff.se))
        l <- l + 1
      }
      res.diff[1, ]$rmtl.diff.se <- NA
      res.diff[1, ]$rmtl.diff.cil <- NA
      res.diff[1, ]$rmtl.diff.ciu <- NA
      res.diff[1, ]$rmtl.diff.p <- NA

      res.diff <- tibble::as_tibble(res.diff) %>%
        dplyr::rename(exposure = .data$label.diff,
                      estimate = .data$rmtl.diff,
                      se = .data$rmtl.diff.se,
                      conf.low = .data$rmtl.diff.cil,
                      conf.high = .data$rmtl.diff.ciu,
                      p.value = .data$rmtl.diff.p)
    }
  } else {
    res.diff <- tibble::tibble()
  }
  list(tau = tau,
       rmtl = tibble::as_tibble(res) %>%
         dplyr::rename(exposure  = .data$groupval,
                       estimate  = .data$rmtl,
                       se        = .data$rmtl.se,
                       conf.low  = .data$cil,
                       conf.high = .data$ciu),
       rmtdiff = res.diff,
       # add origin at (time = 0, cuminc = 0) for each exposure group
       cif = dplyr::bind_rows(tibble::tibble(exposure =
                                               levels(alldat$group)) %>%
                                dplyr::mutate(tj = 0, cif = 0,
                                              se.cif = NA,
                                              cif.cil = NA, cif.ciu = NA),
                              res.cif %>%
                                purrr::transpose() %>%
                                tibble::as_tibble() %>%
                                dplyr::mutate(exposure = names(.data$cif)) %>%
                                tidyr::unnest(cols = c(-.data$exposure))) %>%
         dplyr::select(.data$exposure,
                       time      = .data$tj,
                       estimate  = .data$cif,
                       se        = .data$se.cif,
                       conf.low  = .data$cif.cil,
                       conf.high = .data$cif.ciu))
}
