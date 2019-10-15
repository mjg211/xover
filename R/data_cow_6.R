#' A six-treatment four-period crossover trial on milk production
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 231): "To illustrate,
#' consider the data set that appeared in Table 3.1 of Patterson and Lucas
#' (1962), reproduced in the previous chapter as Table 5.1. Patterson and Lucas
#' rearranged those data into three blocks of four experimental units (cows)
#' each, according to performance in a preliminary period, the results of which
#' were not reported."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 6 treatments;
#' \item 4 periods;
#' \item 12 sequences;
#' \item 12 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 categorical covariate.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 38 rows (observations) and 7
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
#'   \item{block}{A categorical covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#' }
#' @source Patterson HD, Lucas HL (1962) Change-over Designs. \emph{North
#' Carolina Agricultural Experiment Station, Technical Bulletin} \strong{147}.
#' @source Ratkowsky DA, Evans MA, Alldredge JR (1992) \emph{Cross-Over
#' Experiments: Design, Analysis and Application}. Marcel Dekker: New York.
"data_cow_6"
