% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surveymetadatafunctions.R
\name{get_questions_dict}
\alias{get_questions_dict}
\title{get_questions_dict}
\usage{
get_questions_dict(x)
}
\arguments{
\item{x}{a survey data object containing a dpdict, or just a dpdict, with columns for question-level metadata, e.g. questiontype, question_alias}
}
\value{
a questions_dict (subset of dpdict with one row per unique question group)
}
\description{
very simple function to get a questions_dict from a dpdict
}
\examples{
# Using a temp_dpdict
updated_dat <- get_questions_dict(create_dict_with_metadata(get_minimal_labelled_test_dat()))
# Using a survey_data object
updated_dat <- get_questions_dict(create_survey_data(get_minimal_labelled_test_dat()))
}
