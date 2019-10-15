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
#' \item 12 subjects;
#' \item 2 continuous outcome measurements;
#' \item 1 categorical covariate.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 24 rows (observations) and 8
#' columns (variables):
#' \describe{
#'   \item{vas}{A continuous outcome measurement. Stored as an
#'   \code{\link[base]{integer}}.}
#'   \item{pd20}{A continuous outcome measurement. Stored as a
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
#'   \item{`pd20 censored`}{A categorical covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#' }
#' @source ...
"data_asthma_3"
