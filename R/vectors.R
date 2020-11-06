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
#' @param equal Show equal sign if p > 0.001? Defaults to TRUE.
#'
#' @return String: rounded number, preceded by equal sign (unless \code{equal = FALSE})
#'   or less-than sign.
#' @export
#'
#' @examples
#' roundp(0.1138)
#' roundp(c(0.01138, 0.0000138))
#'
#' # Omit equal sign
#' roundp(0.01138, equal = FALSE)
roundp <- function(pee, equal = TRUE) {
  dplyr::case_when(
    pee < 0.001               ~ "< 0.001",
    pee < 0.05 & pee >= 0.001 ~ paste0(if(equal == TRUE) "= ",
                                       format(round(pee, 3), nsmall = 3)),
    pee > 0.05                ~ paste0(if(equal == TRUE) "= ",
                                       format(round(pee, 2), nsmall = 2)))
}
