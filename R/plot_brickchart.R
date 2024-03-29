#' Brick Chart
#'
#' Plot bars of proportions that consist of "bricks" showing individual
#' observations.
#'
#' @param data Data set.
#' @param outcome Outcome expression, e.g., \code{event == TRUE}.
#' @param by Exposure variable.
#' @param group Optional: Grouping variable, e.g., an effect modifier.
#' @param colors Optional: Color list. Must be a \code{list} consisting of
#'   two-element color code vectors with the dark and bright colors for each
#'   level of the exposure variable (\code{by}).
#'   Example: \code{list(c("darkred", "red"), c("darkblue", "lightblue"))}.
#'   If not provided, colors will be generated from the
#'   \code{\link[viridis]{viridis_pal}} palette.
#' @param guide Optional: Show legend? Defaults to \code{FALSE}. May not work
#'   with ggplot version 3.3.4 or newer.
#' @param flip Optional: Flip x and y axes? Defaults to \code{TRUE}.
#' @param clip Optional: Clip graph? Defaults to \code{"on"}.
#' @param ... Optional: further arguments passed to the call of
#'   \code{\link[ggplot2]{facet_grid}}, used for \code{group}.
#'
#' @return ggplot. Modify further with standard ggplot functions. The additional
#'   variables \code{label_outcomes} (outcome count), \code{label_total}
#'   (per-group total), and \code{label_prop} (proportion) can also be accessed.
#'   See example.
#' @export
#'
#' @examples
#' data(cancer, package = "survival")
#' cancer <- cancer %>%
#'   tibble::as_tibble() %>%
#'   dplyr::mutate(sex = factor(sex, levels = 1:2,
#'                              labels = c("Men", "Women")))
#'
#' cancer %>%
#'   dplyr::filter(ph.ecog < 3) %>%  # drop missing/near-empty categories
#'   brickchart(outcome = status == 2,
#'              by = ph.ecog)
#'
#' # Stratified version
#' # Note- Color fill may be off with ggplot v3.3.4+ if guide = TRUE
#' cancer %>%
#'   dplyr::filter(ph.ecog < 3) %>%
#'   brickchart(outcome = status == 2,
#'              by = ph.ecog,
#'              group = sex) +
#'   # Modify graph with standard ggplot functions
#'   # Refer to axes before flipping x <-> y. Here, y is horizontal:
#'   ggplot2::labs(y = "Risk (cumulative incidence)",
#'                 fill = "ECOG status",  # Color label
#'                 title = "Mortality by ECOG performance status") +
#'   # Themes refer to axes as shown--'y' is now vertical
#'   ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
#'   # add label
#'   ggplot2::geom_text(
#'     mapping = ggplot2::aes(
#'       label = paste0(round(label_prop * 100), "%"),
#'       y = label_prop + 0.05))
#'
brickchart <- function(
  data, outcome, by, group,
  colors = NULL,
  guide = FALSE,
  flip = TRUE,
  clip = "on",
  ...) {
  data <- data %>%
    dplyr::mutate({{ by }} := forcats::fct_rev(factor({{ by }})))
  if(missing(group)) {
    group <- NULL  # for facet_grid
  } else {
    data <- data %>%
      dplyr::mutate({{ group }} := factor({{ group }})) %>%
      dplyr::arrange({{ group }})
  }
  if(is.null(colors)) {
    by_length <- length(unique(data %>% dplyr::pull({{ by }})))
    colors <- purrr::map(
      .x = 1:by_length,
      .f = ~c(viridis::viridis_pal(end = 0.9,
                                   option = "cividis",
                                   alpha = 1)(by_length)[.x],
              viridis::viridis_pal(end = 0.9,
                                   option = "cividis",
                                   alpha = 0.8)(by_length)[.x]))
  }
  fillcolors <- data %>%
    dplyr::filter({{ outcome }}) %>%
    dplyr::count({{ by }}) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::left_join(tibble::tibble(colors = colors) %>%
                       dplyr::mutate(index = dplyr::row_number()),
                     by = "index") %>%
    dplyr::mutate(colors = purrr::map2(.x = colors, .y = n,
                                       .f = ~rep(.x, length.out = .y))) %>%
    dplyr::pull(colors) %>%
    purrr::flatten() %>%
    as.character()
  colorguide <- data %>%
    dplyr::filter({{ outcome }}) %>%
    dplyr::group_by({{ by }}) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(index = 100000 * dplyr::row_number() + 1,
                     lbl = {{ by }})
  myplot <- data %>%
    dplyr::mutate(groupnum = 100000 * as.numeric(factor({{ by }}))) %>%
    dplyr::group_by({{ group }}, {{ by }}) %>%
    dplyr::mutate(
      label_total = dplyr::n(),
      proportion = 1 / dplyr::n()) %>%
    dplyr::filter({{ outcome }}) %>%
    dplyr::mutate(
      label_outcomes = dplyr::if_else(
        dplyr::row_number() == 1,
        true = dplyr::n(),
        false = NA_integer_),
      label_prop = dplyr::if_else(
        dplyr::row_number() == 1,
        true = .data$label_outcomes / .data$label_total,
        false = NA_real_),
      label_total = dplyr::if_else(
        dplyr::row_number() == 1,
        true = .data$label_total,
        false = NA_integer_)) %>%
    dplyr::group_by({{ by }}) %>%
    dplyr::mutate(color = factor(dplyr::row_number() + .data$groupnum)) %>%
    dplyr::bind_rows(
      data %>%
        dplyr::distinct({{ group }}) %>%
        dplyr::mutate(
          {{ by }} := (data %>%
                         dplyr::select({{ by }}) %>%
                         dplyr::slice(1) %>%
                         dplyr::pull(1)),
          proportion = 0)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = {{ by }},
                                           y = .data$proportion,
                                           fill = .data$color)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_discrete(drop = FALSE) +
    cowplot::theme_minimal_vgrid() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                   axis.line.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::facet_grid(rows = dplyr::vars({{ group }}), ...)
  if(flip == TRUE) {
    myplot <- myplot +
      ggplot2::coord_flip(clip = clip)
  } else {
    myplot <- myplot +
      ggplot2::coord_cartesian(clip = clip)
  }
  if(guide == FALSE) {
    myplot +
    ggplot2::scale_fill_manual(values = fillcolors, guide = "none")
  } else {
    myplot +
    ggplot2::scale_fill_manual(values = fillcolors,
                               breaks = colorguide$index,
                               labels = colorguide$lbl) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  }
}
