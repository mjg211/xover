#' An AB/BA crossover trial of treatments for heartburn
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 60 subjects;
#' \item 1 continuous and 1 categorical outcome measurement;
#' \item 2 continuous and 2 categorical covariates.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 120 rows (observations) and 11
#' columns (variables):
#' \describe{
#'   \item{md}{A continuous outcome measurement. Stored as an
#'   \code{\link[base]{integer}}.}
#'   \item{cat}{A categorical outcome measurement. Stored as a
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
#'   \item{age}{A continuous covariate. Stored as an
#'   \code{\link[base]{integer}}.}
#'   \item{sex}{A categorical covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#'   \item{freq}{A count covariate. Stored as an
#'   \code{\link[base]{integer}}.}
#'   \item{center}{A categorical covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#' }
#' @source ...
"data_heartburn"
