#' A three treatment, two period, six sequence crossover trial
#'
#' Quoted from : ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 3 treatments;
#' \item 2 periods;
#' \item 6 sequences;
#' \item 18 subjects;
#' \item 1 continuous outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 36 rows (observations) and 6
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
"data_example_5_2"
