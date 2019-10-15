#' A three treatment, three period, six sequence crossover trial of treatments
#' for dysmenorrhea
#'
#' Quoted from Jones and Kenward (2014) (see page ): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 3 treatments;
#' \item 3 periods;
#' \item 6 sequences;
#' \item 86 subjects;
#' \item 1 categorical outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 93 rows (observations) and 6
#' columns (variables):
#' \describe{
#'   \item{relief}{A categorical outcome measurement. Stored as a
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
#' @source Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
"data_dysmenorrhea"
