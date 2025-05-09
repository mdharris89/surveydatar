% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surveymetadatafunctions.R
\name{create_survey_data}
\alias{create_survey_data}
\title{create_survey_data}
\usage{
create_survey_data(dat, dpdict = NULL)
}
\arguments{
\item{dat}{A data frame containing the survey data.}

\item{dpdict}{Optional. A data frame containing the metadata for the survey data. If not provided, it will be created automatically.}
}
\value{
A survey_data object.
}
\description{
Creates a survey_data object, which combines survey data with its metadata.
}
\examples{
dat <- get_minimal_labelled_test_dat()
dpdict <- create_dict(dat)
survey_obj <- create_survey_data(dat, dpdict)
}
