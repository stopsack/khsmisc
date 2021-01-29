# Functions for data handling

#' Variable List, Stata-style
#'
#' @import tidyverse labelled broom cowplot tibble viridis stringr
#'
#' @description This function prints an inventory of a dataset, similar to Stata's
#'   \code{varlist} function.
#'
#' @param data Input data frame (tibble)
#'
#' @return Tibble with the following columns:
#'   * \code{name} Variable name
#'   * \code{n} Number of non-missing observations
#'   * \code{class} Variable class
#'   * \code{label} Variable label
#' @export
#'
#' @examples
#' # Load mtcars dataset,
#' # label it, and create missing values
#' data(mtcars)
#' df <- mtcars %>%
#'   labelled::set_variable_labels(mpg = "Miles per Gallon",
#'                                 gear = "Number of Gears") %>%
#'   dplyr::mutate(qsec = dplyr::if_else(am == 1,
#'                                       true = NA_real_,
#'                                       false = qsec))
#'
#' # Show varlist. Note missing values in "qsec".
#' varlist(df)
varlist <- function(data) {
  labels  <- purrr::map(data, ~base::attr(., "label"))
  tibble::tibble(name  = names(labels),
                 n     = purrr::map_dbl(.x = data, .f = ~sum(!is.na(.))),
                 class = purrr::map_chr(.x = data, .f = class),
                 label = as.character(labels))
}

#' Safely Save CSV File Without Overwriting the File
#'
#' @import readr
#'
#' @description Wraps \code{\link[readr]{write_csv}}. If the file already exists,
#'   it will not be overwritten. A message will be printed indicating whether the file
#'   was successfully written or if it already existed.
#'
#' @param ... Data frame and other arguments,
#'   passed on to \code{\link[readr]{write_csv}}. Required.
#' @param file Path/file name to for output. Required.
#'
#' @return None.
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mtcars %>%
#'   dplyr::select(gear, mpg, am) %>%
#'   write_csv_safely(file = "mtcars.csv")
#' }
write_csv_safely <- function(..., file = stop("'file' must be specified")) {
  if(!file.exists(file)) {
    readr::write_csv(..., file = file)
    print(paste("File written:", file))
  } else
    print(paste("Output CSV file", file, "already exists. Not overwritten."))
}

#' Safely Save R Object Without Overwriting the File
#'
#' @description Wraps \code{\link[base]{save}}. If the file already exists,
#'   it will not be overwritten. A message will be printed indicating whether the file
#'   was successfully written or if it already existed.
#'
#' @param ... Data frame, other objects, and further arguments,
#'   passed on to \code{\link[base]{save}}. Required.
#' @param file Path/file name to for output. Required.
#'
#' @return None.
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mtcars %>%
#'   save_safely(file = "dataset.RData")
#' }
save_safely <- function(..., file = stop("'file' must be specified")) {
  if(!file.exists(file)) {
    save(..., file = file)
    print(paste("File written:", file))
  } else
    print(paste("Output file", file, "already exists. Not overwritten."))
}

#' Safely Export as PDF Without Overwriting the File
#'
#' @description Wraps \code{\link[grDevices]{pdf}}. If the file already exists,
#'   it will not be overwritten. A message will be printed indicating whether the file
#'   was successfully written or if it already existed.
#'
#' @param file Path/file name to for PDF file. Required.
#' @param ... Optional. Passed on to \code{\link[grDevices]{pdf}}.
#'
#' @return None.
#' @export
#'
#' @examples
#' \dontrun{
#' pdf_safely(file = "graphs.pdf")
#' # ... code that generates plots ...
#' dev.off()
#' }
pdf_safely <- function(file = stop("'file' must be specified"), ...) {
  if(!file.exists(file)) {
    grDevices::pdf(file = file, ...)
    print(paste("PDF opened for writing:", file))
  } else
    print(paste("Output PDF file", file, "already exists. Not overwritten."))
}
