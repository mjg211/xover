#' A seven treatment, five period, 21 sequence crossover trial of treatments
#' for asthma
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 7 treatments;
#' \item 5 periods;
#' \item 21 sequences;
#' \item 158 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 period-specific continuous baseline measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 771 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{`log AUC`}{A continuous outcome measurement. Stored as a
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
#' @source ...
"data_asthma_5"
