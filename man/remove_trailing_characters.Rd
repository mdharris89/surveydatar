% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conveniencefunctions.R
\name{remove_trailing_characters}
\alias{remove_trailing_characters}
\title{remove_trailing_characters}
\usage{
remove_trailing_characters(string_to_return, characters_to_remove)
}
\arguments{
\item{string_to_return}{the string to remove characters from}

\item{characters_to_remove}{a vector of characters to remove}
}
\value{
the string_to_return with any characters_to_remove removed from the end
}
\description{
removes a specified set of characters when found (in any order) at the end of a given string
}
\examples{
remove_trailing_characters("SC10ab", c("a", "b"))
}
