#' A four treatment, four period, twelve sequence  crossover trial of treatments
#' for severe respiratory failure
#'
#' Quoted from Jones and Kenward (2014) (see page ): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 4 treatments;
#' \item 4 periods;
#' \item 12 sequences;
#' \item 13 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 continuous period-specific baseline measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 60 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{`oxygen tension`}{A continuous outcome measurement. Stored as a
#'   \code{\link[base]{double}}.}
#'   \item{period}{The period of the trial corresponding to each observation.
#'   Stored as a \code{\link[base]{factor}}.}
#'   \item{treatment}{The administered treatment corresponding to each
#'   observation. Stored as a \code{\link[base]{factor}}.}
#'   \item{subject}{The subject corresponding to each observation. Stored as a
#'   \code{\link[base]{factor}}.}
#'   \item{sequence}{The treatment sequence for the subject corresponding to
#'   each observation. Stored as a \code{\link[base]{factor}}.}
#'   \item{`sequence index`}{The treatment sequence index for the subject
#'   corresponding to each observation. Stored as a \code{\link[base]{factor}}.}
#'   \item{baseline}{A continuous period-specific baseline measurement. Stored as a
#'   \code{\link[base]{double}}.}
#' }
#' @source Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
"data_severe_respiratory_failure"
