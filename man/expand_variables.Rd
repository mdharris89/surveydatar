% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.R
\name{expand_variables}
\alias{expand_variables}
\title{Expand variables and question groups into individual components}
\usage{
expand_variables(
  var_spec,
  data,
  dpdict = NULL,
  statistic_id = NULL,
  values_var = NULL,
  label_mode = "smart"
)
}
\arguments{
\item{var_spec}{Variable specification (can be name, expression, or formula)}

\item{data}{The data frame}

\item{dpdict}{Optional data dictionary}

\item{statistic_id}{The ID of the statistic being calculated}
}
\value{
List of expanded variable specifications
}
\description{
Expand variables and question groups into individual components
}
\keyword{internal}
