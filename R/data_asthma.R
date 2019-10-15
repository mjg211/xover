#' An AB/BA crossover trial of treatments for bronchial asthma
#'
#' Quoted from Jones and Kenward (2014) (see page 55): "These data, which were
#' originally given by Patel (1983), were reported as being taken from the
#' results of a trial involving subjects with mild to acute bronchial asthma.
#' The treatments were single doses of two active drugs and we will label them
#' as A and B, respectively. The response of interest was the forced expired
#' volume in one second (FEV1)."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 17 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 period-specific continuous baseline measurements.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 34 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{fev1}{A continuous outcome measurement. Stored as a
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
#'   \item{`baseline`}{A period-specific continuous baseline measurement. Stored
#'   as a \code{\link[base]{double}}.}
#' }
#' @source Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @source Patel HI (1983) Use of baseline measurements in the two-period
#' cross-over design. Communications in Statistics - Theory and Methods
#' \strong{12:}2693-712
"data_asthma"
