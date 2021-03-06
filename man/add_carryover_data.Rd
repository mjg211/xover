% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_carryover_data.R
\name{add_carryover_data}
\alias{add_carryover_data}
\title{Add carryover information}
\usage{
add_carryover_data(data, summary = T)
}
\arguments{
\item{data}{A \code{\link[tibble]{tibble}} of class \code{xover_data}.}

\item{summary}{A \code{\link[base]{logical}} variable indicating whether a
summary of the function's progress should be printed to the console. Defaults
to \code{T}.}
}
\value{
A \code{\link[tibble]{tibble}} of class \code{xover_data}.
Specifically, the input dataset \code{data} with the addition of a new column
\code{carryover}, stored as a factor with the same levels as
\code{data$treatment}. Note that if \code{data$carryover} is already present
it will be overwritten.
}
\description{
Adds carry-over information to a cross-over dataset.
}
\details{
\code{add_carryover_data()} supports the addition of a column to a cross-over
dataset describing the first order carryover information. Precisely, a
dataset of class \code{xover_data} is provided (see \code{data}) and the new
column is constructed by determining for each outcome variable the treatment
received by that subject in the previous period. Those outcome variables from
period one are assigned the first level of \code{data$treatment}.

Note that datasets can be converted to class \code{xover_data} using
\code{\link[xover]{as_xover_data}}.
}
\references{
Jones B, Kenward MG (2014) \emph{Design and Analysis of
Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
}
\seealso{
\code{\link[xover]{as_xover_data}} for converting datasets to class
\code{xover_data}.
}
