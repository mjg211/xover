#' A three treatment, three period, six sequence crossover trial of treatments
#' for EIA
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 3 treatments;
#' \item 3 periods;
#' \item 6 sequences;
#' \item 30 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 pre-trial continuous baseline measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 90 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{fev1}{A continuous outcome measurement. Stored as a
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
#'   \item{baseline}{A continuous pre-trial baseline measurement. Stored as a
#'   \code{\link[base]{double}}.}
#' }
#' @source ...
"data_eia"
