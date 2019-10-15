#' An AB/BA crossover trial of treatments for cerebrovascular deficiency
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 100 subjects;
#' \item 1 binary outcome measurement;
#' \item 1 categorical covariate.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 200 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{ecg}{A binary outcome measurement. Stored as a
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
#'   \item{center}{A binary covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#' }
#' @source ...
"data_cerebrovascular_deficiency"
