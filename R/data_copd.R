#' An AB/BA crossover trial of treatments for COPD
#'
#' Quoted from Jones and Kenward (2014) (see page 11): "These are data from a
#' single-center, randomized, placebo-controlled, double blind study to evaluate
#' the efficacy and safety of an inhaled drug (A) given twice daily via an
#' inhaler in patients with chronic obstructive pulmonary disease (COPD)."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 58 subjects;
#' \item 2 continuous and 1 categorical outcome measurement;
#' \item 1 pre-trial continuous and 1 pre-trial binary baseline measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 116 rows (observations) and 10
#' columns (variables):
#' \describe{
#'   \item{pefr}{A continuous outcome measurement. Stored as a
#'   \code{\link[base]{double}}.}
#'   \item{nam}{A continuous outcome measurement. Stored as a
#'   \code{\link[base]{double}}.}
#'   \item{severity}{A categorical outcome measurement. Stored as a
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
#'   \item{`baseline nam`}{A continuous pre-trial baseline measurement. Stored
#'   as a \code{\link[base]{double}}.}
#'   \item{`binary baseline nam`}{A binary pre-trial baseline measurement.
#'   Stored as a \code{\link[base]{factor}}.}
#' }
#' @source Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
"data_copd"
