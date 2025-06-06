% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.R
\name{cols_list}
\alias{cols_list}
\title{Create named list of column specifications}
\usage{
cols_list(...)
}
\arguments{
\item{...}{Named expressions for column specifications}
}
\value{
List for use in tab()
}
\description{
Create named list of column specifications
}
\examples{
tab(data, gender, cols = cols_list("Total" = q1, "Young" = q1 * (age < 30)))
}
