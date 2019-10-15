#' A three-treatment seven-period crossover trial
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 239): "Analysis of data
#' from Federer-Atkinson designs is carried outin an identical fashion to the
#' analysis of all other designs in this book, using procedures described in
#' Section 1.1. Consider the data used in the example given in Federer and
#' Atkinson (1964) for a 6 column, 7 row design."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 3 treatments;
#' \item 7 periods;
#' \item 6 sequences;
#' \item 6 subjects;
#' \item 1 continuous outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 42 rows (observations) and 6
#' columns (variables):
#' \describe{
#'   \item{outcome}{A continuous outcome measurement. Stored as a
#'   \code{\link[base]{double}}.}
#'   \item{period}{The period of the trial corresponding to each observation.
#'   Stored as a \code{\link[base]{factor}}.}
#'   \item{subject}{The subject corresponding to each observation. Stored as a
#'   \code{\link[base]{factor}}.}
#'   \item{treatment}{The administered treatment corresponding to each
#'   observation. Stored as a \code{\link[base]{factor}}.}
#'   \item{sequence}{The treatment sequence for the subject corresponding to
#'   each observation. Stored as a \code{\link[base]{factor}}.}
#'   \item{`sequence index`}{The treatment sequence index for the subject
#'   corresponding to each observation. Stored as a \code{\link[base]{factor}}.}
#' }
#' @source Federer WT, Atkinson GF (1964) Tied-double-change-over designs.
#' \emph{Biometrics} \strong{20:}168-81.
#' @source Ratkowsky DA, Evans MA, Alldredge JR (1992) \emph{Cross-Over
#' Experiments: Design, Analysis and Application}. Marcel Dekker: New York.
"data_federer_atkinson"
