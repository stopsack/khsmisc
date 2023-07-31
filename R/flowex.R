#' Perform Sequential Exclusions
#'
#' @description
#' This function performs sequential, user-defined \code{\link[dplyr]{filter}}
#' steps on the input data set. It generates the filtered data and a tibble that
#' can be directly passed on to \code{\link[khsmisc]{exclusion_flowchart}} to
#' plot a flowchart of exclusions.
#'
#' @param criteria Tibble with filtering criteria. Must contain three variables:
#' * \code{left} String with description of data before applying the filter.
#' * \code{right} String with description of data after applying the filter.
#' * \code{filter} Filtering expression quoted using \code{\link[rlang]{expr}}.
#'   The filter in the last row will not be executed, because the last row
#'   serves as a description of the final data set. See examples.
#' @param data Tibble with data set on which the filtering criteria should be
#'   applied.
#'
#' @return
#' A tibble. Each row is a filtering step. Variables:
#' * \code{left}: Labels for included subset that is "left" after the filter.
#' * \code{right}: Labels for excluded subset (which
#'   \code{\link[khsmisc]{exclusion_flowchart}} plots to the right).
#' * \code{included} The data before applying the row's filter.
#' * \code{excluded} The data after applying the row's filter.
#' * \code{n_left} Number of observations before applying the row's filter.
#' * \code{n_right} Number of observations after applying the row's filter.
#'
#' The last row, \code{included}, contains the data after applying all filters.
#' Access this tibble using \code{%>% pull(included) %>% last()}.
#'
#' @export
#'
#' @examples
#' # Example data set
#' data(cancer, package = "survival")
#' cancer <- cancer %>% tibble::as_tibble()
#' cancer
#'
#' # Define exclusion criteria
#' criteria <- tibble::tribble(
#'   ~left,                   ~right,                ~filter,
#'   "All patients",          "Missing ECOG status", expr(!is.na(ph.ecog)),
#'   "Known ECOG",            "Exclude men",         expr(sex == 2),
#'   "Analytical population", "",                    expr(TRUE))
#'
#' # Alternative, equivalent approach to defining the criteria
#' # Note the use of list() around expr(...)
#' criteria <- dplyr::bind_rows(
#'   tibble::tibble(
#'     left = "All patients",
#'     right = "Missing ECOG status",
#'     filter = list(expr(!is.na(ph.ecog)))),
#'   tibble::tibble(
#'     left = "Known ECOG",
#'     right = "Exclude men",
#'     filter = list(expr(sex == 2))),
#'   tibble::tibble(
#'     left = "Analytical population",
#'     right = "",
#'     filter = list(expr(TRUE))))
#'
#' # Perform sequential exclusions
#' result <- make_exclusions(
#'   criteria = criteria,
#'   data = cancer)
#'
#' # Show results
#' result
#'
#' # Access study population after all exclusions
#' result %>%
#'   dplyr::pull(included) %>%
#'   dplyr::last()
#'
#' # Plot flow chart of exclusions (might not display in the online reference)
#' result %>%
#'   exclusion_flowchart()
#'
#' @section Example Output:
#' \if{html}{\figure{make_exclusions.png}{options: width=70\%}}
make_exclusions <- function(criteria, data) {
  criteria %>%
    dplyr::mutate(
      included      = utils::head(purrr::accumulate(
        .x = .data$filter,
        .f = ~dplyr::filter(.data = .x, eval(.y)),
        .init = data), -1),
      included_lead = dplyr::lead(.data$included,
                                  default = .data$included[1]),
      excluded      = purrr::map2(.x = .data$included,
                                  .y = .data$included_lead,
                                  .f = dplyr::setdiff),
      n_left        = purrr::map_int(.x = .data$included,
                                     .f = nrow),
      n_right       = .data$n_left - dplyr::lead(.data$n_left)) %>%
    dplyr::select(-.data$included_lead)
}

#' Exclusion Flowchart Using DiagrammeR
#'
#' @description
#' This function plots a flow chart using \code{\link[DiagrammeR]{grViz}}.
#' It is restricted to the simple case of sequential exclusions from a single
#' study population and not suited for CONSORT flowcharts for a
#' parallel-group study (i.e., a randomized-controlled trial).
#' The \code{left} column describes the flow of the group that ends up
#' being included. The \code{right} column describes exclusions for various
#' reasons.
#'
#' Generate the \code{design} using \code{\link[khsmisc]{make_exclusions}}.
#'
#' @param design Tibble with the following columns:
#'   * \code{left} Text for the left box of each row.
#'   * \code{n_left} Count to be shown as 'n =' in the bottom
#'     of the left box. To skip, use \code{NA_integer_}.
#'   * \code{right} Text for the right box of each row.
#'   * \code{n_right} Count to be shown as 'n =' in the bottom
#'     of the right box. To skip, use \code{NA_integer_}.
#' @param width Minimum width for all boxes. Defaults to \code{3} (inches).
#'
#' @details
#' Note \code{\link[DiagrammeR]{grViz}} does not automatically
#' generate line breaks. To avoid extra-wide boxes, manually
#' supply line breaks using \code{\\n}. See example.
#'
#' @return Plot rendered using \code{\link[DiagrammeR]{grViz}}.
#'   For a \code{design} with three rows,
#'   the following pseudo-code would be generated: \preformatted{
#'   grViz(paste0("digraph flowchart {
#'     node [fontname = Helvetica, shape = rectangle, width = 4]
#'     tab1a [label = 'left[1] \\nn = ", n_left[1],  "']
#'     tab1b [label = 'right[1]\\nn = ", n_right[1], "']
#'     tab2a [label = 'left[2] \\nn = ", n_left[2],  "']
#'     tab2b [label = 'right[2]\\nn = ", n_right[2], "']
#'     tab3a [label = 'left[3] \\nn = ", n_left[3],  "']
#'
#'     { rank = same; tab1a; tab1b; }
#'     { rank = same; tab2a; tab2b; }
#'
#'     tab1a -> tab2a -> tab3a;
#'     tab1a -> tab1b;
#'     tab2a -> tab2b;
#'     }"))
#'   }
#' @export
#'
#' @examples
#' # Generate a flow chart for two steps of exclusions:
#' design <- tibble::tribble(
#'   ~left,               ~n_left, ~right,              ~n_right,
#'   "Study base",        1000,    "Not sampled",       250,
#'   "Study population",  750,     "Participants with\nmissing exposure data", 100,
#'   "Complete-case set", 650,     "",                  NA_integer_)
#'
#' # Plot
#' exclusion_flowchart(design, width = 2)
#'
#' @section Example Output:
#' \if{html}{\figure{exclusion_flowchart.png}{options: width=50\%}}
exclusion_flowchart <- function(design, width = 3) {
  labels <- design %>%
    dplyr::mutate(
      index = dplyr::row_number(),
      text = purrr::pmap_chr(
        .l = list(.data$index, .data$left, .data$n_left),
        .f = ~paste0("tab", ..1, "a [label = '", ..2,
                     dplyr::if_else(!is.na(..3),
                                    true = paste0("\\nn = ", ..3),
                                    false = ""),
                     "']")),
      text = purrr::pmap_chr(
        .l = list(.data$index, .data$text, .data$right, .data$n_right),
        .f = ~dplyr::if_else(
          ..3 == "",
          true = ..2,
          false = paste0(..2, "\ntab", .x, "b [label = '", ..3,
                         dplyr::if_else(!is.na(..4),
                                        true = paste0("\\nn = ", ..4),
                                        false = ""),
                         "']"))))
  labels <- paste(labels$text, sep = "\n", collapse = "\n")

  # Rows that have a right box
  has_right <- design %>%
    dplyr::mutate(row_number = dplyr::row_number()) %>%
    dplyr::filter(.data$right != "") %>%
    dplyr::pull(.data$row_number)

  DiagrammeR::grViz(paste0(
    "digraph flowchart {
      node [fontname = Helvetica, shape = rectangle, width = ", width, "]\n",
    labels,
    paste0("{ rank = same; tab", has_right, "a; tab", has_right, "b; }",
           sep = "\n", collapse = "\n"),
    paste0(paste0("tab", 1:(nrow(design)-1), "a -> ", collapse = " "),
           "tab", nrow(design), "a;\n"),
    paste0("tab", has_right, "a -> tab", has_right, "b;",
           sep = "\n", collapse = "\n"),
    "}"))
}
