#' A six sequence crossover trial of treatments for migraine
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 3 treatments;
#' \item 3 periods;
#' \item 6 sequences;
#' \item 33 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 binary covariate.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 99 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{vas}{A continuous outcome measurement. Stored as an
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
#'   \item{`substituted data`}{A binary covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#' }
#' @source ...
"data_migraine"
