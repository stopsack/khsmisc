#' Box and Whiskers/Jittered Dot Plot
#'
#' @import ggplot2
#'
#' @description Plots categorical x-axis and continuous y-axis.
#'   Inspired by Nick Cox's Stata plug-in \code{stripplot}. As per
#'   \code{\link[ggplot2]{geom_boxplot}}, boxes reach from the first to the
#'   third quartile; whiskers extend 1.5 times the interquartile range
#'   but not beyond the most extreme data point.
#'
#' @param data Data frame. Required.
#' @param x Categorical variable for x-axis. Required.
#' @param y Continuous variable for y-axis. Required.
#' @param contrast If added, the mean difference between extreme categories
#'   will be added. \code{contrast} provides the preceding label. See example.
#'   Defaults to \code{NULL} (do not show contrast).
#' @param unit Scale to print after the point estimate for the
#'   contrast between extreme categories.
#'   Defaults to \code{NULL} (no unit).
#' @param digits Number of digits for rounding point estimate and
#'   confidence intervals of the contrast. Defaults to 2.
#' @param jitter Avoid overplotting of points with similar values?
#'   Defaults to \code{TRUE}.
#' @param color Variable to color data points by. Defaults to \code{NULL} (none).
#' @param na.rm Remove "NA" category from x-axis?
#'   Defaults to \code{FALSE}.
#' @param printplot \code{print()} the plot? Defaults to \code{FALSE},
#'   i.e., just return the plot.
#'
#' @return ggplot object, or nothing
#'   (if plot is sent to graphics device with \code{printplot = TRUE}).
#'   Standard customization options for a \code{ggplot} object can be added
#'   on afterwards; see example.
#' @export
#'
#' @examples
#' data(mtcars)
#'
#' # Basic plot
#' mtcars %>%
#'   stripplot(x = "gear", y = "mpg")
#'
#' # Add mean difference between extreme categories, reduce digits,
#' # add color by 'wt', add different color scale, and label,
#' # all using standard ggplot syntax.
#' mtcars %>%
#'   stripplot(x = "gear", y = "mpg",
#'   contrast = "5 vs. 3 gears", unit = "mpg\n", digits = 1, color = "wt") +
#'   scale_color_viridis(option = "cividis") +
#'   labs(y = "Miles per gallon", color = "Weight")
stripplot <- function(data, x, y,
                      contrast  = NULL,
                      unit      = NULL,
                      digits    = 2,
                      jitter    = TRUE,
                      color     = NULL,
                      na.rm     = FALSE,
                      printplot = FALSE) {
  xlbl <- labelled::var_label(dplyr::pull(data, x))
  ylbl <- labelled::var_label(dplyr::pull(data, y))
  if(is.null(xlbl))
    xlbl <- x
  if(is.null(ylbl))
    ylbl <- y
  if(na.rm == TRUE)
    data <- data %>% dplyr::filter(!is.na(get(x)))

  myplot <- ggplot2::ggplot(data, aes(factor(get(x)), get(y))) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    labs(x = xlbl, y = ylbl)
  if (jitter == TRUE) {
    set.seed(3457)
    if(is.null(color))
      myplot <- myplot + ggplot2::geom_jitter(height = 0, width = 0.2)
    else
      myplot <- myplot + ggplot2::geom_jitter(height = 0, width = 0.2,
                                              mapping = ggplot2::aes(color = get(color)))
  } else {
    if(is.null(color))
      myplot <- myplot + ggplot2::geom_point()
    else
      myplot <- myplot + ggplot2::geom_point(mapping = ggplot2::aes(color = get(color)))
  }

  if(!is.null(contrast)) {
    fit <- stats::lm(paste0(y, " ~ as.factor(", x, ")"), data = data)
    estlab <- paste0(
      contrast,
      ", ",
      format(round(utils::tail(stats::coef(fit), n = 1),
                   digits = digits), nsmall = digits),
      " ",
      if_else(is.na(unit), "", paste0(unit, " ")),
      "(95% CI, ",
      format(round(utils::tail(stats::confint.default(fit), n = 1)[1],
                   digits = digits), nsmall = digits),
      " to ",
      format(round(utils::tail(stats::confint.default(fit), n = 1)[2],
                   digits = digits), nsmall = digits),
      ")")
    myplot <- myplot + ggplot2::annotate(geom = "text", label = estlab,
                                         x = Inf, y = Inf, hjust = 1, vjust = 1)
  }
  myplot <- myplot +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.line          = element_blank(),
                   panel.grid.major.y = element_line(color = "gray80"),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.x = element_blank(),
                   axis.ticks         = element_blank())
  if(printplot == TRUE)
    print(myplot)
  else
    return(myplot)
}
