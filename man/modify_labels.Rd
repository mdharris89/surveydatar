% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.R
\name{modify_labels}
\alias{modify_labels}
\title{Modify row and column labels using pattern matching}
\usage{
modify_labels(x, row_labels = NULL, col_labels = NULL)
}
\arguments{
\item{x}{A data frame, typically a tab_result object}

\item{row_labels}{Named character vector of pattern/replacement pairs for row labels.
Patterns are applied sequentially to the row_label column.}

\item{col_labels}{Named character vector of pattern/replacement pairs for column names.
Patterns are applied sequentially to column names (excluding row_label).}
}
\value{
The modified data frame with updated labels, preserving original class and attributes
}
\description{
Apply gsub pattern replacements to row labels and/or column names in a data frame.
Particularly useful for formatting tab_result objects before visualization.
}
\examples{
# Basic usage
result <- modify_labels(
  tab_result,
  row_labels = c("Very satisfied" = "Very sat", "Somewhat satisfied" = "Somewhat sat"),
  col_labels = c("Male 18-34" = "M 18-34", "Female 35+" = "F 35+")
)

# In a pipeline
my_tab \%>\%
  modify_labels(row_labels = c("\\\\s+" = " ")) \%>\%  # Remove extra spaces
  tab_to_flourish()
}
