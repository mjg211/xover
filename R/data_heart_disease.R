#' An AB/BA crossover trial of treatments for heart disease
#'
#' Quoted from Jones and Kenward (2014) (see page ): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 30 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 continuous pre-trial baseline measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 60 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{duration}{A continuous outcome measurement. Stored as an
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
#' @source Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
"data_heart_disease"
