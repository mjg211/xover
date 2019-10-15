#' An eight treatment, sixteen period, ## sequence crossover trial on perceived
#' speed
#'
#' Quoted from Jones and Kenward (2014) (see page ): ""
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 8 treatments;
#' \item 16 periods;
#' \item ## sequences;
#' \item 16 subjects;
#' \item 1 continuous outcome measurement;
#' \item 2 categorical covariates.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 256 rows (observations) and 8
#' columns (variables):
#' \describe{
#'   \item{`perceived speed`}{A continuous outcome measurement. Stored as a
#'   \code{\link[base]{double}}.}
#'   \item{period}{The period of the trial corresponding to each observation.
#'   Stored as a \code{\link[base]{factor}}.}
#'   \item{treatment}{The administered treatment corresponding to each
#'   observation. Stored as a \code{\link[base]{factor}}.}
#'   \item{subject}{The subject corresponding to each observation. Stored as a
#'   \code{\link[base]{factor}}.}
#'   \item{sequence}{The treatment sequence for the subject corresponding to
#'   each observation. Stored as a \code{\link[base]{factor}}.}
#'   \item{`sequence index`}{The treatment sequence index for the subject
#'   corresponding to each observation. Stored as a \code{\link[base]{factor}}.}
#'   \item{display}{A categorical covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#'   \item{speed}{A categorical covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#' }
#' @source Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
"data_perceived_speed"
