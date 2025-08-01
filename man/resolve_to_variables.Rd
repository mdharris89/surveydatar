% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resolve-vars.R
\name{resolve_to_variables}
\alias{resolve_to_variables}
\title{Resolve a name to variable names in data}
\usage{
resolve_to_variables(
  name,
  data,
  dpdict = NULL,
  allow_patterns = TRUE,
  error_context = "variable or group"
)
}
\arguments{
\item{name}{Character string to resolve}

\item{data}{Data frame to search in}

\item{dpdict}{Optional data dictionary for question group lookup}

\item{allow_patterns}{Logical, whether to try pattern matching}

\item{error_context}{Character string describing what's being resolved (for error messages)}
}
\value{
Character vector of variable names
}
\description{
Takes a name and resolves it to one or more variable names by checking:
\enumerate{
\item Exact variable match in data
\item Pattern match for variable stems (optional)
\item Question group match in dpdict (if provided)
}
}
\keyword{internal}
