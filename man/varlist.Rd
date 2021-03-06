% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datahandling.R
\name{varlist}
\alias{varlist}
\title{Variable List, Stata-style}
\usage{
varlist(data)
}
\arguments{
\item{data}{Input data frame (tibble)}
}
\value{
Tibble with the following columns:
\itemize{
\item \code{name} Variable name
\item \code{n} Number of non-missing observations
\item \code{class} Variable class
\item \code{label} Variable label
}
}
\description{
This function prints an inventory of a dataset, similar to Stata's
\code{varlist} function.
}
\examples{
# Load mtcars dataset,
# label it, and create missing values
data(mtcars)
df <- mtcars \%>\%
  labelled::set_variable_labels(mpg = "Miles per Gallon",
                                gear = "Number of Gears") \%>\%
  dplyr::mutate(qsec = dplyr::if_else(am == 1,
                                      true = NA_real_,
                                      false = qsec))

# Show varlist. Note missing values in "qsec".
varlist(df)
}
