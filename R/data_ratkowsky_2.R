#' An AB/BA crossover trial with a binary outcome measurement
#'
#' Taken from Ratkowsky \emph{et al.} (1992) (see page 313).
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 100 subjects;
#' \item 1 binary outcome measurement
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 200 rows (observations) and 6
#' columns (variables):
#' \describe{
#'   \item{outcome}{A binary outcome measurement. Stored as an
#'   \code{\link[base]{factor}}.}
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
"data_ratkowsky_2"
