#' A four-period two-treatment crossover trial
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 385): "."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 4 periods;
#' \item 2 sequences;
#' \item 24 subjects;
#' \item 1 continuous outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 96 rows (observations) and 6
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
#' @source Gill JL (1978) \emph{Design and Analysis of Experiments in the Animal
#' and Medical Sciences}, Volume 2. The Iowa State University Press: Iowa.
#' @source Ratkowsky DA, Evans MA, Alldredge JR (1992) \emph{Cross-Over
#' Experiments: Design, Analysis and Application}. Marcel Dekker: New York.
"data_cow_7"
