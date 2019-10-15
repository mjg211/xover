#' A four treatment, four period, four sequence crossover trial
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 4 treatments;
#' \item 4 periods;
#' \item 4 sequences;
#' \item 80 subjects;
#' \item 1 binary outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 320 rows (observations) and 6
#' columns (variables):
#' \describe{
#'   \item{outcome}{A binary outcome measurement. Stored as a
#'   \code{\link[base]{factor}}.}
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
"data_example_6_1"
