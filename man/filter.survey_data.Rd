% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surveymetadatafunctions.R
\name{filter.survey_data}
\alias{filter.survey_data}
\title{Filter method for survey_data objects}
\usage{
\method{filter}{survey_data}(.data, ...)
}
\arguments{
\item{.data}{A survey_data object.}

\item{...}{Filter conditions passed to dplyr::filter.}
}
\value{
A new survey_data object with filtered data and unchanged metadata.
}
\description{
Subsets rows of the survey data based on conditions, preserving metadata.
}
\examples{
survey_obj <- create_survey_data(get_minimal_labelled_test_dat())
# Filter rows where uid > 5
filtered <- dplyr::filter(survey_obj, uid > 5)
}
