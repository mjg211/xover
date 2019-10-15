#' An AB/BA crossover trial on milk-yield
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 115): "Let us now
#' consider a set of data from a feeding trial conducted by the Department of
#' Primary Industry, Tasmania, the purpose of which was to test whether dairy
#' cows could be fed, and thrive upon, a waste product from cheese-making known
#' as delactose permeate."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 8 subjects;
#' \item 1 period-specific continuous baseline measurement;
#' \item 1 post-trial continuous outcome measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 18 rows (observations) and 8
#' columns (variables):
#' \describe{
#'   \item{`milk yield`}{A continuous outcome measurement. Stored as an
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
#'   \item{baseline}{A period-specific continuous baseline measurement. Stored
#'   as a \code{\link[base]{double}}.}
#'   \item{baseline}{A post-trial continuous outcome measurement. Stored
#'   as a \code{\link[base]{double}}.}
#' }
#' @source Ratkowsky DA, Evans MA, Alldredge JR (1992) \emph{Cross-Over
#' Experiments: Design, Analysis and Application}. Marcel Dekker: New York.
"data_cow_2"
