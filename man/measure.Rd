\name{measure}
\alias{measure}
\title{Create a Measure Specification}
\usage{
measure(statistic, values = NULL, id = NULL, label = NULL)
}
\arguments{
\item{statistic}{Statistic identifier (character) or \code{tab_stat} object.}

\item{values}{Optional values variable name for value-based statistics.}

\item{id}{Optional stable id for the measure. If omitted, tab() creates one.}

\item{label}{Optional display label for the measure block.}
}
\value{
A \code{tab_measure} object for use in \code{tab(measures = list(...))}.
}
\description{
Constructs one explicit measure specification used by multi-measure \code{tab()} calls.
}
