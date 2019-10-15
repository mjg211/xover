#' A two treatment, two period, four sequence crossover trial on treatments for
#' parkinsons
#'
#' Quoted from : ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 4 sequences;
#' \item 17 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 continuous pre-trial baseline measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 34 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{`scores total`}{A continuous outcome measurement. Stored as a
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
#'   \item{baseline}{A continuous pre-trial baseline measurement. Stored as an
#'   \code{\link[base]{integer}}.}
#' }
#' @source ...
"data_parkinsons"
