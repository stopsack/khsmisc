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
#'   stripplot(x = gear, y = mpg)
#'
#' # Add mean difference between extreme categories, reduce digits,
#' # add color by 'wt', add different color scale, and label,
#' # all using standard ggplot syntax.
#' mtcars %>%
#'   stripplot(x = gear, y = mpg,
#'   contrast = "5 vs. 3 gears", unit = "mpg\n", digits = 1, color = wt) +
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
  x <- names(data %>% dplyr::select({{ x }}))
  y <- names(data %>% dplyr::select({{ y }}))
  color <- names(data %>% dplyr::select({{ color }}))
  xlbl <- labelled::var_label(dplyr::pull(data, x))
  ylbl <- labelled::var_label(dplyr::pull(data, y))
  if(is.null(xlbl))
    xlbl <- x
  if(is.null(ylbl))
    ylbl <- y
  if(na.rm == TRUE) {
    data <- data %>%
      dplyr::filter(!is.na(get(x)))
  }

  myplot <- ggplot2::ggplot(data = data,
                            mapping = ggplot2::aes(x = factor(get(x)),
                                                   y = get(y))) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    labs(x = xlbl, y = ylbl) +
    cowplot::theme_minimal_hgrid() +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank())
  if (jitter == TRUE) {
    set.seed(3457)
    if(length(color) == 0)
      myplot <- myplot + ggplot2::geom_jitter(height = 0, width = 0.2)
    else
      myplot <- myplot + ggplot2::geom_jitter(height = 0, width = 0.2,
                                              mapping = ggplot2::aes(color = get(color)))
  } else {
    if(length(color) == 0)
      myplot <- myplot + ggplot2::geom_point()
    else
      myplot <- myplot + ggplot2::geom_point(mapping = ggplot2::aes(color = get(color)))
  }

  if(!is.null(contrast)) {
    fit <- stats::lm(formula = yvar ~ xvar,
                     data = data %>%
                       dplyr::rename(xvar = dplyr::one_of(x),
                                     yvar = dplyr::one_of(y)) %>%
                       dplyr::mutate(xvar = as.factor(.data$xvar)))
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
  if(printplot == TRUE)
    print(myplot)
  else
    return(myplot)
}


#' Correlation plot
#'
#' @description Performs pairwise Pearson or Spearman correlations.
#'
#' @param data Required. Data frame. Only numeric variables will be used;
#'   variables of other types will silently be dropped. To include
#'   categorical variables, coerce factors to numeric first. See examples.
#' @param ... Optional. Variables to correlate. If not provided,
#'   all numeric variables will be used. Supports tidy evaluation;
#'   see examples.
#' @param method Optional. Correlation method. Defaults to \code{"pearson"}.
#' @param use How \code{\link[stats]{cor}} handles
#'   missing values. Defaults to a restriction to complete
#'   observations with all variables. See parameter \code{use} of
#'   \code{\link[stats]{cor}} for alternatives.
#' @param reorder Perform hierarchical clustering in the correlation matrix?
#'   This will order variables by their correlation patterns with other variables.
#'   If turned off to \code{FALSE}, the order of variables in the dataset will be retained.
#'   Defaults to \code{TRUE}.
#' @param digits Decimal digits for displayed correlation coefficients. Defaults to \code{2}.
#' @param legendpos (x, y) coordinates of color legend. Use
#'   \code{legendpos = "none"} to turn off the legend.
#'   Defaults to \code{c(0.15, 0.35)}.
#' @param cutpoints Correlation coefficient values that have a distinct
#'   color. Defaults to \code{c(-1, 0, 1)}.
#' @param colors Colors for the \code{cutpoints}. Defaults to blue (for negative),
#'   white (no correlation), and yellow (positive correlation) on the
#'   \code{cividis} color scale.
#'
#' @return ggplot. Can be modified with the usual ggplot commands, such as
#'   \code{\link[ggplot2]{theme}}.
#' @export
#'
#' @seealso \url{http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization}
#' @examples
#' data(mtcars)
#'
#' mtcars %>%
#'   corrmat(mpg, cyl, hp, wt, qsec)
#'
#' # Equivalent:
#' mtcars %>%
#'   select(mpg, cyl, hp, wt, qsec) %>%
#'   corrmat()
#'
#' # Can use tidy evaluation to select variables:
#' mtcars %>%
#'   corrmat(contains("a"), starts_with("c"))
#'
#' # If "cyl" was a character, it would be excluded:
#' mtcars %>%
#'   mutate(cyl_chr = as.character(cyl)) %>%
#'   corrmat(mpg, cyl_chr, hp, wt, qsec)
#'
#' # To retain the character variable "cyl",
#' # convert to factor and then make numeric:
#' mtcars %>%
#'   mutate(cyl_chr = as.character(cyl),
#'          cyl_chr = as.numeric(factor(cyl_chr))) %>%
#'   corrmat(mpg, cyl_chr, hp, wt, qsec)
corrmat <- function(
  data,
  ...,
  method    = "pearson",
  use       = "pairwise.complete.obs",
  reorder   = TRUE,
  digits    = 2,
  legendpos = c(0.15, 0.35),
  cutpoints = c(-1, 0, 1),
  colors    = c(viridis_pal(option = "cividis")(10)[1],     # for lowest value of scale
                "white",                                    # middle
                viridis_pal(option = "cividis")(10)[8])) {  # highest

  # matrix reorder by hierarchical clustering (optional)
  reorder_cormat <- function(cormat) {
    # Use correlation between variables as distance
    dd <- stats::as.dist((1 - cormat) / 2)
    hc <- stats::hclust(dd)
    cormat <- cormat[hc$order, hc$order]
  }

  # Get upper triangle of the correlation matrix
  get_upper_triangle <- function(cormat) {
    cormat[base::lower.tri(cormat)] <- NA
    return(cormat)
  }

  newdata <- data %>% select(!!!rlang::enquos(...))
  if(ncol(newdata) == 0)
    newdata <- data
  data <- newdata

  mymat <- data %>%
    dplyr::select_if(is.numeric) %>%
    stats::cor(method = method, use = use)
  if(reorder == TRUE)
    mymat <- mymat %>% reorder_cormat()
  mymat %>%
    get_upper_triangle() %>%
    tibble::as_tibble(rownames = "var1") %>%
    tidyr::pivot_longer(cols = -.data$var1, names_to = "var2", values_to = "value",
                        values_drop_na = TRUE) %>%
    dplyr::mutate_at(.vars = vars(.data$var1, .data$var2),
                     .funs = ~forcats::fct_inorder(factor(.))) %>%
    dplyr::group_by(.data$var1) %>%
    dplyr::mutate(ylabel = dplyr::if_else(dplyr::row_number() == 1,
                                          true = as.character(.data$var1), false = "")) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(mapping = aes(.data$var2, .data$var1, fill = .data$value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low      = colors[1],
                         high     = colors[3],
                         mid      = colors[2],
                         midpoint = cutpoints[2],
                         limit    = c(cutpoints[1], cutpoints[3]),
                         space    = "Lab",
                         name     = stringr::str_to_title(paste(method,
                                                                "correlation", sep = "\n"))) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::geom_text(aes(x = .data$var2, y = .data$var1,
                           label = format(round(.data$value, digits = digits),
                                          nsmall = digits)),
                       color = "black", size = 4) +
    ggplot2::geom_text(mapping = aes(x = as.numeric(.data$var2) - 0.7,
                                     y = as.numeric(.data$var1),
                                     label = .data$ylabel),
              hjust = 1) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::theme(axis.title.x     = element_blank(),
                   axis.title.y     = element_blank(),
                   axis.text.x      = element_text(angle = 45, vjust = 1,
                                                   size = 12, hjust = 1),
                   axis.text.y      = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.border     = element_blank(),
                   panel.background = element_blank(),
                   axis.line        = element_blank(),
                   axis.ticks       = element_blank(),
                   legend.position  = legendpos,
                   plot.margin      = margin(t = 0, r = 0, b = 0, l = 2, unit = "cm"),
                   legend.direction = "vertical",
                   legend.justification = c(1, 0)) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 1, barheight = 5,
                                                   title.position = "top", title.hjust = 0.5))
}
