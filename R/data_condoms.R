#' A six sequence crossover trial of condoms
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 3 treatments;
#' \item 3 periods;
#' \item 6 sequences;
#' \item 36 subjects;
#' \item 2 count outcome measurements;
#' \item 2 count covariates.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 108 rows (observations) and 9
#' columns (variables):
#' \describe{
#'   \item{breaks}{A count outcome measurement. Stored as an
#'   \code{\link[base]{integer}}.}
#'   \item{n}{A count outcome measurement. Stored as an
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
#'   \item{condoms}{A count covariate. Stored as an
#'   \code{\link[base]{integer}}.}
#'   \item{periods}{A count covariate. Stored as an
#'   \code{\link[base]{integer}}.}
#' }
#' @source ...
"data_condoms"
