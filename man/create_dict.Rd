% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surveymetadatafunctions.R
\name{create_dict}
\alias{create_dict}
\title{create_dict}
\usage{
create_dict(temp_dat, prefill = TRUE)
}
\arguments{
\item{temp_dat}{a dataframe, assumed to be survey data}

\item{prefill}{if TRUE (the default), columns variable_names/variable_labels/value_labels will be populated with values from 'old' versions. if FALSE, will leave as NA.}
}
\value{
a tibble with columns for old_variable_names, old_variable_labels, old_value_labels, and new_ versions of each
}
\description{
creates a tibble to use as a dictionary to easily update variable names and labels
}
\details{
note: uses sjlabelled to get value
}
\examples{
create_dict(get_minimal_labelled_test_dat())

}
