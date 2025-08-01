% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.R
\name{sync_base_matrix}
\alias{sync_base_matrix}
\title{Sync base matrix with current row and column arrays}
\usage{
sync_base_matrix(row_arrays, col_arrays, base_array, values_array, statistic)
}
\arguments{
\item{row_arrays}{List of row arrays}

\item{col_arrays}{List of column arrays}

\item{base_array}{Base array for filtering}

\item{values_array}{Optional values array for mean calculations}

\item{statistic}{The statistic object containing base_calculator}
}
\value{
Matrix of base values with dimensions matching row/col arrays
}
\description{
This function recalculates the base matrix whenever the dimensions change,
ensuring consistency between arrays and bases. It applies gates during
base calculation to maintain orthogonal structure.
}
\keyword{internal}
