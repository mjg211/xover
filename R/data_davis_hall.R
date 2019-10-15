#' A six-treatment three-period crossover trial
#'
#' Quoted from Ratkowsky \emph{et al.} (1992) (see page 235): "Analysis of the
#' Davis-Hall cyclic designs is carried out in exactly the same way as the
#' analysis of other cross-over designs, balanced or unbalanced, using the
#' approach described in Section 1.1. Let us consider the set of data reported
#' in Table 2 of Davis and Hall (1969)."
#'
#' Data pertains to a trial with
#'
#' \itemize{
#' \item 6 treatments;
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
#'   \item{block}{A categorical covariate. Stored as a
#'   \code{\link[base]{factor}}.}
#' }
#' @source Davis AW, Hall WB (1969) Cyclic change-over designs.
#' \emph{Biometrika} \strong{56:}283-93.
#' @source Ratkowsky DA, Evans MA, Alldredge JR (1992) \emph{Cross-Over
#' Experiments: Design, Analysis and Application}. Marcel Dekker: New York.
"data_davis_hall"
