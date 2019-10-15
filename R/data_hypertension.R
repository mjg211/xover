#' A two treatment, three period, four sequence crossover trial on treatments
#' for hypertension
#'
#' Quoted from : ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 3 periods;
#' \item 4 sequences;
#' \item 89 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 continuous pre-trial baseline measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 34 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{`blood pressure`}{A continuous outcome measurement. Stored as an
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
#'   \item{baseline}{A continuous pre-trial baseline measurement. Stored as an
#'   \code{\link[base]{integer}}.}
#' }
#' @source ...
"data_hypertension"
