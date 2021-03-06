% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_balaam.R
\name{seq_balaam}
\alias{seq_balaam}
\title{Balaam (1968) cross-over design specification}
\usage{
seq_balaam(D = 3, labels = 0:(D - 1), as_matrix = T, summary = T)
}
\arguments{
\item{D}{The number of treatments. Must be a single
\code{\link[base]{numeric}} integer greater than or equal to two. Defaults to
\code{3}.}

\item{labels}{A \code{\link[base]{vector}} of labels for the treatments.
Should be of \code{\link[base]{length}} \code{D}, containing unique elements.
Defaults to \code{0:(D - 1)}.}

\item{as_matrix}{A \code{\link[base]{logical}} variable indicating whether
the design should be returned as a \code{\link[base]{matrix}}, or a
\code{\link[tibble]{tibble}}. Defaults to \code{T}.}

\item{summary}{A \code{\link[base]{logical}} variable indicating whether a
summary of the function's progress should be printed to the console. Defaults
to \code{T}.}
}
\value{
Either a \code{\link[base]{matrix}} if \code{as_matrix = T} (with
rows corresponding to sequences and columns to periods), or a
\code{\link[tibble]{tibble}} if \code{as_matrix = F} (with rows corresponding
to a particular period on a particular sequence). In either case, the
returned object will have class \code{xover_seq}.
}
\description{
Specifies cross-over designs from Balaam (1968).
}
\details{
\code{seq_balaam()} supports the specification of cross-over designs from
Balaam (1968). Designs for three through six treatments (see \code{D}) are
supported, for any chosen treatment labels (see \code{labels}). In addition,
the designs can be returned in \code{\link[base]{matrix}} or
\code{\link[tibble]{tibble}} form (see \code{as_matrix}).

Precisely, the \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
element of the cross-over design matrix corresponds to the treatment a
subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
}
\examples{
# Balaam (1968) design for three treatments
balaam        <- seq_balaam()
# Using different labels
balaam_ABC    <- seq_balaam(labels = LETTERS[1:3])
# Returning in tibble form
balaam_tibble <- seq_balaam(as_matrix = F)
}
\references{
Balaam LN (1968) A two-period design with \emph{t}² experimental
units. \emph{Biometrics} \strong{24:}61-73.
}
\author{
Based on data from the \code{\link[Crossover]{Crossover}} package by
Kornelius Rohmeyer.
}
