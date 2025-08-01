% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conveniencefunctions.R
\name{convert_integers_to_double}
\alias{convert_integers_to_double}
\title{Convert Integer Variables to Double While Preserving Labels}
\usage{
convert_integers_to_double(x, labelled_only = TRUE)
}
\arguments{
\item{x}{A vector, data frame, or survey_data object}

\item{labelled_only}{Logical. If TRUE, only converts integers that have labels.
If FALSE, converts all integers. Default TRUE.}
}
\value{
Object of same type as input with integers converted to doubles
}
\description{
Converts integer variables to double storage type while preserving all
labelling attributes. Useful for ensuring consistent variable types when
integer vs double differences cause issues with grouping functions.
}
\examples{
# For individual vector
int_var <- structure(c(1L, 2L, 3L),
                     class = c("haven_labelled", "vctrs_vctr", "integer"),
                     labels = c("Low" = 1, "High" = 3))
double_var <- convert_integers_to_double(int_var)

# For data frame
df <- data.frame(x = 1:3, y = c(1.1, 2.2, 3.3))
df_converted <- convert_integers_to_double(df)
}
