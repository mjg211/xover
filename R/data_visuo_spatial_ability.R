#' An AB/BA crossover trial of visuo spatial ability
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 23 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 continuous covariate.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 46 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{`log eft`}{A continuous outcome measurement. Stored as a
#'   \code{\link[base]{double}}.}
#'   \item{iq}{A continuous covariate. Stored as a
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
"data_visuo_spatial_ability"
