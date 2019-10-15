#' A two-period crossover trial on milk production in cows
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 185): "Consider the
#' following data on milk production in kg/day for an incomplete block design
#' with four treatments, 12 subjects, and two periods."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 4 treatments;
#' \item 2 periods;
#' \item 12 sequences;
#' \item 12 subjects;
#' \item 1 continuous outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 24 rows (observations) and 6
#' columns (variables):
#' \describe{
#'   \item{production}{A continuous outcome measurement. Stored as a
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
#' @source Ratkowsky DA, Evans MA, Alldredge JR (1992) \emph{Cross-Over
#' Experiments: Design, Analysis and Application}. Marcel Dekker: New York.
"data_cow_5"
