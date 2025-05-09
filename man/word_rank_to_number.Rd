% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conveniencefunctions.R
\name{word_rank_to_number}
\alias{word_rank_to_number}
\title{word_rank_to_number}
\usage{
word_rank_to_number(word_string)
}
\arguments{
\item{word_string}{a word number, e.g. "one" or ranking word, e.g. "first"}
}
\value{
the numeric associated with the given word number or ranking word
}
\description{
accepts a word number or ranking word, e.g. "one" or "first", and returns the associated numeric, for numbers 1-50
}
\examples{
word_rank_to_number("first")
word_rank_to_number("one")
}
