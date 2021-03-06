% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table2.R
\name{table2}
\alias{table2}
\title{Table 2: Stratified Result Tables}
\usage{
table2(
  design,
  data,
  layout = "rows",
  factor = 1000,
  risk_percent = FALSE,
  diff_digits = dplyr::if_else(risk_percent == TRUE, true = 0, false = 2),
  ratio_digits = 2,
  rate_digits = 1,
  to = NULL
)
}
\arguments{
\item{design}{Design matrix (data frame) that sets up the table.
See Details.}

\item{data}{Dataset to be used for all analyses.}

\item{layout}{Optional. \code{"rows"} uses the \code{design} as rows and
exposure categories as columns. \code{"cols"} is the
opposite: \code{design} as columns and exposure categories as rows.
Defaults to \code{"rows"}.}

\item{factor}{Optional. Used for \code{type = "rates"}: Factor to multiply
events per person-time by. Defaults to \code{1000}.}

\item{risk_percent}{Optional. Show risk and risk difference estimates in
percentage points instead of proportions. Defaults to \code{FALSE}.}

\item{diff_digits}{Optional. Number of decimal digits to show for
rounding of means, risks, and risk difference estimates. Defaults to \code{2}
for \code{risk_percent = FALSE} and to \code{0} for
\code{risk_percent = TRUE}. Can override for each line in \code{type}.}

\item{ratio_digits}{Optional. Number of decimal digits to show for ratio
estimates. Defaults to \code{2}. Can override for each line in \code{type}.}

\item{rate_digits}{Optional. Number of decimal digits to show for rates.
Defaults to \code{1}. Can override for each line in \code{type}.}

\item{to}{Optional. Separator between the lower and the upper bound
of the 95\% confidence interval (and interquartile range for medians).
Defaults to \code{" to "} for means, medians, and mean differences;
defaults to \code{"-"} otherwise.}
}
\value{
Tibble. Get formatted output by passing on to
\code{\link[khsmisc]{mygt}}.
}
\description{
This function displays descriptive
and inferential results for binary, continuous, and survival data
in the format of a table stratified by exposure and, if requested, by
effect modifiers.

This function is intended only for tabulations of final results.
Model diagnostics for regression models need to be conducted separately.
}
\details{
The main input parameter is the dataset \code{design}.
Always required are the columns \code{label}, \code{type}, and
\code{exposure}, as well as \code{outcome} for binary outcomes
or \code{time} and \code{event} for survival outcomes:
\itemize{
\item \code{label} A label for each row (or column).
\item \code{time} The time variable for survival data. Needed for,
e.g., \code{type = "hr"} and \code{type = "rate"}
(i.e., whenever \code{outcome} is not used).
\item \code{time2} The second time variable for late entry models.
Only used in conjunction with \code{time}. If provided,
\code{time} will become the entry time and \code{time2}
the exit time, following conventions of
\code{\link[survival]{Surv}}.
\item \code{event} The event variable for survival data.
Events are typically \code{1}, censored observations \code{0}.
Needed for, e.g., \code{type = "hr"} and \code{type = "rate"}
(i.e., whenever \code{outcome} is not used).
\item \code{outcome} The outcome variable for non-survival data
(i.e., whenever \code{event} and \code{time} are not used).
For risk/prevalence data, this variable must be \code{0}/\code{1}
or \code{FALSE}/\code{TRUE}.
\item \code{exposure} The exposure variable. Must be categorical
(factor or logical).
\item \code{trend} Optional. A continuous representation of the
exposure, for which a slope per one unit increase ("trend") will be
estimated. Must be a numeric variable. If joint models for
\code{exposure} and \code{effect_modifier} are requested,
trends are still reported within each stratum of the
\code{effect_modifier}. Use \code{NA} to leave blank.
\item \code{effect_modifier} Optional. A categorical effect modifier variable.
Use \code{NA} to leave blank.
\item \code{stratum} Optional. A stratum of the effect modifier.
Use \code{NULL} to leave blank. \code{NA} will evaluate
observations with missing data for the \code{effect_modifier}.
\item \code{confounders} Optional. A string in the format
\code{"+ var1 + var2"} that will be substituted into
into \code{formula = exposure + confounders}.
Use \code{NA} or \code{""} (empty string) to leave blank; the default.
For Cox models, can add \code{"+ strata(site)"}
to obtain models with stratification by, e.g., \code{site}.
For Poisson models, can add \code{"+ offset(log(persontime))"}
to define, e.g., \code{persontime} as the offset.
\item \code{type} The statistic requested (case-insensitive):

Comparative estimates from regression models
with 95\% confidence intervals:
\itemize{
\item \code{"hr"} Hazard ratio from Cox proportional
hazards regression.
\item \code{"irr"} Incidence rate ratio for count outcomes
from Poisson regression model.
\item \code{"rr"} Risk ratio (or prevalence ratio)
from \code{\link[risks]{riskratio}}. Can request specific model
fitting  approach and, for marginal
standardization only, the number of bootstrap repeats.
Examples: \code{"rrglm_start"} or \code{"rrmargstd2000"}.
(Note absence of white space.)
\item \code{"rd"} Risk difference (or prevalence difference)
from \code{\link[risks]{riskdiff}}. Can request model fitting approach
and bootstrap repeats as for \code{"rr"}.
\item \code{"diff"} Mean difference from linear model.
\item \code{"quantreg"} Quantile difference from quantile regression using
\code{\link[quantreg]{rq}} with \code{method = "fn"}.
By default, this is the difference in medians. For a different
quantile, e.g., the 75th percentile, use \code{"quantreg0.75"}.
Note absence of white space before the quantile.
\item \code{"fold"} Fold change from generalized linear
model with log link (i.e., ratio of arithmetic means).
\item \code{"foldlog"} Fold change from linear
model after log transformation of the outcome
(i.e., ratio of geometric means).
\item \code{"or"} Odds ratio from logistic regression.
}

Absolute estimates per exposure category:
\itemize{
\item \code{"events"} Event count.
\item \code{"time"} Person-time.
\item \code{"outcomes"} Outcome count.
\item \code{"total"} Number of observations.
\item \code{"events/time"} Events slash person-time.
\item \code{"events/total"} Events slash number of observations.
\item \code{"cases/controls"} Cases and non-cases;
for case-control studies.
\item \code{"risk"} Risk (or prevalence), i.e., events divided
by number of observations. Change between display as proportion
or percent using the parameter \code{risk_percent}.
\item \code{"risk (ci)"} Risk with 95\% confidence interval
(Wilson score interval for binomial proportions, see
\code{\link[khsmisc]{scoreci}}).
\item \code{"rate"} Event rate: event count divided by person-time,
multiplied by \code{factor}.
\item \code{"rate (ci)"} Event rate with 95\% confidence interval
(Poisson-type interval, see \code{\link[khsmisc]{rates}}).
\item \code{"outcomes (risk)"} A combination: Outcomes
followed by risk in parentheses.
\item \code{"outcomes/total (risk)"} A combination: Outcomes slash total
followed by risk in parentheses.
\item \code{"events/time (rate)"} A combination: Events slash time
followed by rate in parentheses.
\item \code{"mean"} Mean.
\item \code{"mean (ci)"} Mean and 95\% CI.
\item \code{"median"} Median.
\item \code{"median (iqr)"} Median and interquartile range.
\item \code{"blank"} or \code{""} An empty line.
}

By default, regression models will be fit separately for each
stratum of the \code{effect_modifier}. Append \code{"_joint"}
to \code{"hr"}, \code{"rr"}, \code{"rd"}, \code{"irr"}, \code{"diff"},
\code{"fold"}, \code{"foldlog"}, \code{"quantreg"}, or \code{"or"} to
obtain "joint" models for exposure and effect modifier that have a
single reference category.
Example: \code{type = "hr_joint"}. The reference categories
for exposure and effect modifier are their first factor levels, which
can be changed using \code{\link[forcats]{fct_relevel}}.

Digits for rounding estimates can be specified for each line separately.
Example: \code{type = "diff (ci) 3"} to request a mean difference
and its 95\% CI rounded to 3 decimal digits (note the space before
\code{3}).
}

Hint: Use \code{\link[tibble]{tibble}}, \code{\link[tibble]{tribble}}, and
\code{\link[dplyr]{mutate}} to construct the \code{design} dataset,
especially variables that are used repeatedly (e.g., \code{exposure, time,
  event}, or \code{outcome}). See examples.
}
\examples{
# Load 'cancer' dataset from survival package
data(cancer, package = "survival")

# The exposure (here, 'sex') must be categorical
cancer <- cancer \%>\%
  tibble::as_tibble() \%>\%
  dplyr::mutate(sex = factor(sex, levels = 1:2,
                             labels = c("Men", "Women")),
                time = time / 365.25,
                status = status - 1)

# Example 1: Binary outcomes (use 'outcome' variable)
# Set table design
design1 <- tibble::tibble(
  label = c("Outcomes",
            "Total",
            "Outcomes/Total",
            "Risk",
            "Risk (CI)",
            "Outcomes (Risk)",
            "Outcomes/Total (Risk)",
            "RR",
            "RD")) \%>\%
  dplyr::mutate(type = label,
                exposure = "sex",
                outcome = "status")

# Generate table2
table2(design = design1, data = cancer)

# Use 'design' as columns (selecting RR and RD only)
table2(design = design1 \%>\%
                  dplyr::filter(label \%in\% c("RR", "RD")),
       data = cancer,
       layout = "cols")

# Example 2: Survival outcomes (use 'time' and 'event'),
#   with an effect modifier and a confounder
# Set table design
design2 <- tibble::tribble(
  # Elements that vary by row:
  ~label,                       ~stratum, ~confounders, ~type,
  "Overall: Events",            NULL,     "",           "events",
  "  Person-years",             NULL,     "",           "time",
  "  Rate/1000 py (95\% CI)",    NULL,     "",           "rate (ci)",
  "  Unadjusted HR (95\% CI)",   NULL,     "",           "hr",
  "  Age-adjusted HR (95\% CI)", NULL,     "+ age",      "hr",
  "",                           NULL,     "",           "blank",
  "Stratified models",          NULL,     "",           "blank",
  "ECOG PS1 (events/N)",        1,        "",           "events/total",
  "  Unadjusted",               1,        "",           "hr",
  "  Age-adjusted",             1,        "+ age",      "hr",
  "ECOG PS2 (events/N)",        2,        "",           "events/total",
  "  Unadjusted",               2,        "",           "hr",
  "  Age-adjusted",             2,        "+ age",      "hr",
  "",                           NULL,     "",           "",
  "Joint model, age-adj.",      NULL,     "",           "",
  "  ECOG PS1",                 1,        "+ age",      "hr_joint",
  "  ECOG PS2",                 2,        "+ age",      "hr_joint") \%>\%
  # Elements that are the same for all rows:
  dplyr::mutate(exposure = "sex",
                event = "status",
                time = "time",
                effect_modifier = "ph.ecog")

# Generate table2
table2(design = design2,
       data = cancer \%>\% dplyr::filter(ph.ecog \%in\% 1:2))

# Example 3: Continuous outcomes (use 'outcome' variable);
# request rounding to 1 decimal digit in some cases;
# add continuous trend (slope per one unit of the 'trend' variable)
tibble::tribble(
  ~label,                   ~stratum, ~type,
  "Marginal mean (95\% CI)", NULL,     "mean (ci) 1",
  "  Men",                  "Men",    "mean",
  "  Women",                "Women",  "mean",
  "",                       NULL,     "",
  "Stratified model",       NULL,     "",
  "  Men",                  "Men",    "diff 1",
  "  Women",                "Women",  "diff 1",
  "",                       NULL,     "",
  "Joint model",            NULL,     "",
  "  Men",                  "Men",    "diff_joint",
  "  Women",                "Women",  "diff_joint") \%>\%
  dplyr::mutate(exposure = "ph.ecog_factor",
                trend = "ph.ecog",
                outcome = "age",
                effect_modifier = "sex") \%>\%
  table2(data = cancer \%>\%
                  dplyr::filter(ph.ecog < 3) \%>\%
                  dplyr::mutate(ph.ecog_factor = factor(ph.ecog)))

# Get formatted output:
\dontrun{
table2(design = design2, data = cancer) \%>\%
  mygt()
}
}
