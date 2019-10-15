#' A four treatment, four period, four sequence crossover trial of formoterol
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 4 treatments;
#' \item 4 periods;
#' \item 4 sequences;
#' \item 16 subjects;
#' \item 1 count outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 64 rows (observations) and 6
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
#' }
#' @source ...
"data_formoterol"
