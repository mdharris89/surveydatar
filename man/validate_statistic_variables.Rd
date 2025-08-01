% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.R
\name{validate_statistic_variables}
\alias{validate_statistic_variables}
\title{Validate Variable Types for Statistics}
\usage{
validate_statistic_variables(
  statistic,
  rows_expanded,
  cols_expanded,
  data,
  dpdict = NULL
)
}
\arguments{
\item{statistic}{A tab_stat object containing statistic metadata}

\item{rows_expanded}{List of expanded row variable specifications}

\item{cols_expanded}{List of expanded column variable specifications}

\item{data}{Data frame being analyzed}
}
\value{
NULL if validation passes, throws error with helpful suggestions if not
}
\description{
Validates that variables used in cross-tabulation rows and columns are
appropriate for the specified statistic. For value-based statistics like
mean, ensures variables are categorical rather than continuous numeric.
}
\details{
For statistics requiring values (like mean), this function prevents users from
accidentally creating meaningless cross-tabs with many numeric categories.
Variables with >15 unique values trigger errors with suggestions to use
factor(), cut(), add value labels, or use correlation statistics instead.
}
\keyword{internal}
