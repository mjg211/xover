#' An AB/BA crossover trial of treatments for EIA
#'
#' Quoted from Senn (2002): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 24 subjects;
#' \item 1 ordinal and 1 binary outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 48 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{`4ps`}{An ordinal outcome measurement. Stored as a
#'   \code{\link[base]{factor}}.}
#'   \item{`2ps`}{A binary outcome measurement. Stored as a
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
"data_eia_2"
