#' A two treatment, four period, two sequence crossover trial of phenytoin
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 4 periods;
#' \item 2 sequences;
#' \item 26 subjects;
#' \item 2 continuous outcome measurements.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 104 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{AUC}{A continuous outcome measurement. Stored as a
#'   \code{\link[base]{double}}.}
#'   \item{Cmax}{A continuous outcome measurement. Stored as a
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
"data_phenytoin"
