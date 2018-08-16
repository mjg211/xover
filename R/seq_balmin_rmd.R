#' Afsarinejad (1983) cross-over design specification
#'
#' Specifies cross-over designs from Afsarinejad (1983).
#'
#' \code{seq_balmin_rmd()} supports the specification of cross-over designs from
#' Afsarinejad (1983). Designs for any number of treatments (see \code{D}) are
#' supported, for any chosen treatment labels (see \code{labels}). The number of
#' periods (see \code{J}) can be any number such that
#' \ifelse{html}{\out{(<i>D</i> - 1)/(<i>J</i> - 1)}}{\eqn{(D - 1)/(J - 1)}} is
#' a positive integer. In addition, the designs can be returned in
#' \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form (see
#' \code{as_matrix}).
#'
#' Precisely, a balanced minimal repeated measurement design is returned. The
#' sequences are incomplete, i.e., no sequence contains all of the treatments.
#' The design is balanced for carryover effects but will in general not be a
#' balanced block design. A necessary and sufficient condition for the existence
#' of such a design is that
#' \ifelse{html}{\out{(<i>D</i> - 1)/(<i>J</i> - 1)}}{\eqn{(D - 1)/(J - 1)}} is
#' a positive integer: this is therefore enforced by the function. The number of
#' sequences, \ifelse{html}{\out{<i>K</i>}}{\eqn{K}}, in the resulting design is
#' \ifelse{html}{\out{<i>D</i>(<i>D</i> - 1)/(<i>J</i> - 1)}}{\eqn{
#' D(D - 1)/(J - 1)}}. Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' cross-over design matrix corresponds to the treatment a subject on the
#' \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to two. Defaults to
#' \code{2}.
#' @param J The number of periods. Must be a single \code{\link[base]{numeric}}
#' integer less than or equal to \code{D}. Defaults to \code{2}.
#' @param labels A \code{\link[base]{vector}} of labels for the treatments.
#' Should be of \code{\link[base]{length}} \code{D}, containing unique elements.
#' Defaults to \code{0:(D - 1)}.
#' @param as_matrix A \code{\link[base]{logical}} variable indicating whether
#' the design should be returned as a \code{\link[base]{matrix}}, or a
#' \code{\link[tibble]{tibble}}. Defaults to \code{T}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return Either a \code{\link[base]{matrix}} if \code{as_matrix = T} (with
#' rows corresponding to sequences and columns to periods), or a
#' \code{\link[tibble]{tibble}} if \code{as_matrix = F} (with rows corresponding
#' to a particular period on a particular sequence). In either case, the
#' returned object will have class \code{xover_seq}.
#' @examples
#' # Afsarinejad (1983) design for four treatments and two periods
#' balmin_rmd        <- seq_balmin_rmd()
#' # Using different labels
#' balmin_rmd_ABCD   <- seq_balmin_rmd(labels = LETTERS[1:4])
#' # Returning in tibble form
#' balmin_rmd_tibble <- seq_balmin_rmd(as_matrix = F)
#' @references Afsarinejad K (1983) Balanced repeated measurements designs.
#' \emph{Biometrika} \strong{70:}199-204.
#' @references Wakeling IN, MacFie HJH (1995) Designing consumer trials balanced
#' for first and higher orders of carry-over effect when only a subset of k
#' samples from t may be tested. \emph{Food Qual Prefer} \strong{6:}299-308.
#' @author Based on code from the \code{\link[crossdes]{crossdes}} package by
#' Oliver Sailer.
#' @export
seq_balmin_rmd <- function(D = 2, J = 2, labels = 0:(D - 1), as_matrix = T,
                           summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(1, Inf), 1)
  check_integer_range(J, "J", c(1, D + 1), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  if (((D - 1)/(J - 1))%%1 != 0) {
    stop("(D - 1)/(J - 1) must be a positive integer for a balanced minimal ",
         "repeated measurements design to exist")
  }
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  K     <- D*(D - 1)/(J - 1)
  vec   <- numeric(D)
  if (!(D%%2)) {
    for (i in 1:(D/2)) {
      vec[(2*i - 1):(2*i)] <- c(i, D + 1 - i)
    }
  } else {
    for (i in 1:(D + 1)%/%4) {
      vec[(2*i - 1):(2*i)] <- c(2*i - 1, D + 2 - 2*i)
    }
  }
  dummy <- 2*((D + 1)%/%4)
  if (!((D - 1)%%4)) {
    vec <- c(vec[1:dummy], (D + 1)/2, rev(vec[1:dummy]))
  }
  if (!((D + 1)%%4)) {
    vec <- c(vec[1:dummy], rev(vec[1:(dummy - 1)]))
  }
  sequences                      <- matrix(0, nrow = J, ncol = K)
  for (i in 1:(K/D)) {
    for (j in 1:D) {
      sequences[, D*(i - 1) + j] <- (vec[((J - 1)*(i - 1) + 1):
                                           ((J - 1)*i + 1)] + j - 1)%%D
    }
  }
  sequences[!sequences]          <- D
  sequences                      <- t(sequences)
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  sequences                      <- convert_labels(sequences, D, labels, 1:D)
  sequences                      <- transform_to_xover(sequences, labels,
                                                       as_matrix)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
