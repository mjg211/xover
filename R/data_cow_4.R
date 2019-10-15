#' A two-period crossover trial on methionine requirements in dairy calves
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 184): "Consider the
#' following two-period cross-over experiment used to study methionine
#' requirements in dairy calves. Suppose 5 doses are studies (A = 70%, B = 85%,
#' C = 100%, D = 115%, E = 130% of methionine content of milk). Gill (1978),
#' Exercise 8.11, pp. 248-9, considered putting 20 calves in ten
#' \ifelse{html}{\out{2 &times; 2}}{\eqn{2 \times 2}} Latin squares to permit a
#' balanced design."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 5 treatments;
#' \item 2 periods;
#' \item 20 sequences;
#' \item 20 subjects;
#' \item 1 continuous outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 40 rows (observations) and 6
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
"data_cow_4"
