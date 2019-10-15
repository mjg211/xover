#' An AB/BA crossover trial of asthma treatments
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 13 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 categorical covariate and 1 period-specific continuous baseline
#' measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 26 rows (observations) and 8
#' columns (variables):
#' \describe{
#'   \item{pef}{A continuous outcome measurement. Stored as an
#'   \code{\link[base]{integer}}.}
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
#'   \item{sex}{A categorical covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#'   \item{baseline}{A continuous period-specific baseline measurement. Stored
#'   as an \code{\link[base]{integer}}.}
#' }
#' @source ...
"data_asthma_2"
