% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surveymetadatafunctions.R
\name{create_questions_dict}
\alias{create_questions_dict}
\title{create_questions_dict}
\usage{
create_questions_dict(survey_obj = NULL, temp_dpdict = NULL, editfirst = TRUE)
}
\arguments{
\item{survey_obj}{a survey_data object, or NULL, in which case requires a temp_dpdict}

\item{temp_dpdict}{a dpdict}

\item{editfirst}{whether to call data_edit before returning, for any manual edits}
}
\value{
a questions_dict with questions metadata corresponding to the given dpdict
}
\description{
creates questions_dict from a dpdict. if dpdict doesn't already has a questions_lcs column, creates one.
}
\details{
optionally calls data_edit before returning, for any manual edits.
}
\examples{
# Using a dpdict
temp_dpdict <- create_dict_with_metadata(get_big_test_dat(n=10))
questions_dict <- create_questions_dict(NULL, temp_dpdict, editfirst = FALSE)

# Using a survey_data object
survey_obj <- create_survey_data(get_big_test_dat(n=10))
questions_dict <- create_questions_dict(survey_obj, editfirst = FALSE)
}
