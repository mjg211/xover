#' A four-treatment crossover trial on rabbits
#'
#' Taken from Ratkowsky \emph{et al.} (1992) (see page 70). Originally from
#' Bliss (1967).
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 4 treatments;
#' \item 4 periods;
#' \item 9 sequences;
#' \item 12 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 categorical covariate.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 48 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{outcome}{A continuous outcome measurement. Stored as an
#'   \code{\link[base]{integer}}.}
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
#'   \item{square}{A categorical covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#' }
#' @source Bliss CI (1967) \emph{Statistics in Biology}, Volume 1. McGraw Hill:
#' New York.
#' @source Ratkowsky DA, Evans MA, Alldredge JR (1992) \emph{Cross-Over
#' Experiments: Design, Analysis and Application}. Marcel Dekker: New York.
"data_rabbit"
