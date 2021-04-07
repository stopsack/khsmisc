#' Brick Chart
#'
#' Plot bars of proportions that consist of "bricks" showing individual
#' observations.
#'
#' @param data Data set
#' @param outcome Outcome expression, e.g., \code{event == TRUE}
#' @param by Exposure variable
#' @param group Optional: Grouping variable, e.g. an effect modifier.
#' @param colors Color list. Must be a \code{list} consisting of two-element
#'   color code vectors with the dark and bright colors. The default
#'   provides colors for three exposure groups; additions may be necessary.
#'   Example: \code{list(c("darkred", "red"), c("darkblue", "lightblue"))}.
#' @param guide Optional: Show legend? Defaults to \code{FALSE}.
#'
#' @return ggplot. Modify further with standard ggplot functions.
#' @export
#'
#' @examples
#' data(ovarian, package = "survival")
#' ovarian %>%
#'   brickchart(outcome = fustat == 1,
#'              by = ecog.ps)
#' ovarian %>%
#'   brickchart(outcome = fustat == 1,
#'              by = ecog.ps,
#'              group = rx)
brickchart <- function(
  data, outcome, by, group,
  colors = list(
    c(viridis::viridis_pal(end = 0.9, option = "cividis", alpha = 1)(3)[1],
      viridis::viridis_pal(end = 0.9, option = "cividis", alpha = 0.8)(3)[1]),
    c(viridis::viridis_pal(end = 0.9, option = "cividis", alpha = 1)(3)[2],
      viridis::viridis_pal(end = 0.9, option = "cividis", alpha = 0.8)(3)[2]),
    c(viridis::viridis_pal(end = 0.9, option = "cividis", alpha = 1)(3)[3],
      viridis::viridis_pal(end = 0.9, option = "cividis", alpha = 0.8)(3)[3])),
  guide = FALSE) {
  data <- data %>% dplyr::mutate({{ by }} := forcats::fct_rev(factor({{ by }})))
  if(missing(group))
    group <- NULL  # for facet_grid
  else
    data <- data %>% dplyr::arrange({{ group }})
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
    dplyr::mutate(proportion = 1 / dplyr::n()) %>%
    dplyr::filter({{ outcome }}) %>%
    dplyr::group_by({{ by }}) %>%
    dplyr::mutate(color = factor(dplyr::row_number() + .data$groupnum)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = {{ by }},
                                           y = .data$proportion,
                                           fill = .data$color)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    cowplot::theme_minimal_vgrid() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                   axis.line.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::facet_grid(rows = dplyr::vars({{ group }}))
  if(guide == FALSE) {
    myplot +
    ggplot2::scale_fill_manual(values = fillcolors, guide = FALSE)
  } else {
    myplot +
    ggplot2::scale_fill_manual(values = fillcolors,
                               breaks = colorguide$index,
                               labels = colorguide$lbl) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  }
}
