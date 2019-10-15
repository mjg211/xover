#' A six-treatment crossover trial on testicular diffusing factor
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 76): "The following set
#' of data came from a study of the effect of the order of administration of a
#' set of six treatments on testicular diffusing factor. Six rabbits were
#' employed in this study, using six positions on the backs of each of these
#' rabbits."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 6 treatments;
#' \item 6 periods;
#' \item 6 sequences;
#' \item 6 subjects;
#' \item 1 continuous outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 36 rows (observations) and 6
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
"data_testicular"
