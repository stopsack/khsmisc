# Functions mainly for single vectors

#' Scale Numeric Vector to a Given Range
#'
#' @param x Numeric vector
#' @param min Optional. Desired new minimum value. Defaults to 0.
#' @param max Optional. Desired new maximum value. Defaults to 1.
#' @param ... Optional. Passed on to \code{min()} and \code{max()}
#'   functions internally. If vector contains missing values,
#'   may want to specify \code{na.rm = TRUE}.
#'
#' @return Numeric vector with minimum value of \code{min} and
#'   maximum value of \code{max}.
#' @export
#'
#' @examples
#' # Simple numeric vector:
#' x <- c(-2, 0, 4)
#'
#' # Scale to the c(0, 1) range:
#' scale_to_range(x)
#'
#' # Leave minimum value untouched:
#' scale_to_range(x, min = -2, max = 1)
scale_to_range <- function(x, min = 0, max = 1, ...) {
  ((x - min(x, ...)) * (max - min) / (max(x, ...) - min(x, ...))) + min
}

#' Most Frequent Observation
#'
#' @description Not to be confused with \code{\link[base]{mode}} in lowercase,
#'   a \code{base} R function that returns the storage mode of an object.
#'
#' @param x Vector
#'
#' @return Scalar: most frequent value.
#' @export
#'
#' @examples
#' Mode(c(1, "b", "b", 0.3))
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Round and Format p-Values
#'
#' @import tidyverse
#'
#' @description Following conventions of many medical journals, this function
#'   rounds a p-value to two digits (if p > 0.05) or three digits (if 0.001 <= p < 0.05),
#'   or returns an inequality of "< 0.001" (if p < 0.001).
#'
#' @param pee Numeric p-vale
#' @param equal Show equal sign if p > 0.001? Defaults to \code{FALSE}
#'
#' @return String: rounded number, preceded by equal sign (unless \code{equal = FALSE})
#'   or less-than sign.
#' @export
#'
#' @examples
#' roundp(0.1138)
#' roundp(c(0.01138, 0.0000138))
#'
#' # Include equal sign
#' roundp(0.01138, equal = TRUE)
roundp <- function(pee, equal = FALSE) {
  dplyr::case_when(
    pee < 0.001               ~ "< 0.001",
    pee < 0.05 & pee >= 0.001 ~ paste0(if(equal == TRUE) "= ",
                                       format(round(pee, 3), nsmall = 3)),
    pee > 0.99                ~ "> 0.99",
    pee > 0.05                ~ paste0(if(equal == TRUE) "= ",
                                       format(round(pee, 2), nsmall = 2)))
}


#' Wilson Score Confidence Intervals
#'
#' @description
#' "This function computes a confidence interval for a proportion.
#' It is based on inverting the large-sample normal score test for the
#' proportion." (Alan Agresti, who wrote the original R code)
#'
#' Inputs for \code{success}, \code{total}, and \code{level}
#' are vectorized.
#'
#' @param success Success count.
#' @param total Total count.
#' @param level Optional. Confidence level. Defaults to 0.95.
#' @param return_midpoint Optional. Return midpoint of confidence
#'   interval? Defaults to \code{FALSE}.
#'
#' @return Data frame:
#'   * \code{success} Success count
#'   * \code{total} Total count
#'   * \code{estimate} Proportion
#'   * \code{conf.low} Lower bound of the confidence interval.
#'   * \code{conf.high} Upper bound of the confidence interval.
#'   * \code{midpoint} Mid-point of the confidence interval
#'     (for \code{return_midpoint = TRUE}).
#'   * \code{level} Confidence level.
#' @noRd
#'
#' @seealso
#' \url{http://users.stat.ufl.edu/~aa/cda/R/one-sample/R1/index.html}
#'
#' Agresti A, Coull BA. Approximate is better than "exact" for
#' interval estimation of binomial proportions, Am Stat 1998;52:119-126.
#'
#' Brown LD, Cai TT, DasGupta A. Interval estimation for a
#' binomial proportion (with discussion), Stat Sci 2001;16:101-133.
#'
#' @examples
#' scoreci(success = 5, total = 10)
#' scoreci(success = c(5:10), total = 10, level = 0.9)
scoreci <- function(success, total, level = 0.95, return_midpoint = FALSE) {
  zalpha <- abs(qnorm((1 - level) / 2))
  estimate <- success / total
  bound <- (zalpha * ((estimate * (1 - estimate) + (zalpha**2) / (4 * total)) / total)**(1 / 2)) /
    (1 + (zalpha**2) / total)
  midpoint <- (estimate + (zalpha**2) / (2 * total)) / (1 + (zalpha**2) / total)

  conf.low  <- midpoint - bound
  conf.high <- midpoint + bound

  if(return_midpoint)
    data.frame(success, total, estimate, conf.low, conf.high, midpoint, level)
  else
    data.frame(success, total, estimate, conf.low, conf.high, level)
}
