#' A four-treatment three-period crossover trial on milk production
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 167): "The 12
#' experimental units were divided up at random into four blocks of three cows
#' each and the three sequences within each square were separately allocated to
#' cows at random. Periods were five weeks long and the response variable is the
#' average daily production of fat-corrected milk (FCM)."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 4 treatments;
#' \item 3 periods;
#' \item 12 sequences;
#' \item 12 subjects;
#' \item 1 continuous outcome measurement;
#' \item 1 categorical covariate.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 36 rows (observations) and 7
#' columns (variables):
#' \describe{
#'   \item{fcm}{A continuous outcome measurement. Stored as a
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
"data_cow_3"
