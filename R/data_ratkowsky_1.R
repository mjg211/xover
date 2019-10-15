#' An AB/BA crossover trial on drug efficacy
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 118): "Consider the
#' following set of data for comparing a new drug (B) with a standard
#' formulation (A). Patients were randomly allocated to two sequences AB and BA,
#' respectively. Baseline readings were taken before application of the first
#' drug, and during a wash-out period between the two treatment periods. The
#' following results were obtained on a scale with a maximum value of 15."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 2 treatments;
#' \item 2 periods;
#' \item 2 sequences;
#' \item 29 subjects;
#' \item 1 period-specific continuous baseline measurement.
#' }
#'
#' @format A \code{\link[tibble]{tibble}} with 40 rows (observations) and 7
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
#'   \item{baseline}{A period-specific continuous baseline measurement. Stored
#'   as an \code{\link[base]{integer}}.}
#' }
#' @source Ratkowsky DA, Evans MA, Alldredge JR (1992) \emph{Cross-Over
#' Experiments: Design, Analysis and Application}. Marcel Dekker: New York.
"data_ratkowsky_1"
