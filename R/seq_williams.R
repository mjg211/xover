#' Williams cross-over design specification
#'
#' Specifies Williams cross-over designs.
#'
#' \code{seq_williams()} supports the specification of Williams designs.
#' Sequences for any number of treatments (see \code{D}) are supported, for any
#' chosen treatment labels (see \code{labels}). In addition, the designs can be
#' returned in \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form
#' (see \code{as_matrix}).
#'
#' Precisely, Williams designs are (generalized) Latin squares that are balanced
#' for first order carryover effects. Generally, carryover balance is achieved
#' with very few sequences. Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' cross-over design matrix corresponds to the treatment a subject on the
#' \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to two. Defaults to
#' \code{2}.
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
#' # A Williams design for three treatments
#' williams        <- seq_williams(D = 3)
#' # Using different labels
#' williams_ABC    <- seq_williams(D = 3, labels = LETTERS[1:3])
#' # Returning in tibble form
#' williams_tibble <- seq_williams(D = 3, as_matrix = F)
#' @references Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @references Wakeling IN, MacFie HJH (1995) Designing consumer trials balanced
#' for first and higher orders of carry-over effect when only a subset of k
#' samples from t may be tested. \emph{Food Qual Prefer} \strong{6:}299-308.
#' @references Williams EJ (1949) Experimental designs balanced for the
#' estimation of residual effects of treatments. \emph{Aust J Sci Res Ser A}
#' \strong{2:}149-168.
#' @author Based on code from the \code{\link[crossdes]{crossdes}} package by
#' Oliver Sailer.
#' @export
seq_williams <- function(D = 2, labels = 0:(D - 1), as_matrix = T,
                         summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(1, Inf), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (D == 2) {
    sequences        <- matrix(c(0, 1, 1, 0), 2, 2)
  } else {
    k_1              <- c(0, rep(1, D - 1))
    factor           <- ifelse((3:D)%%2 == 0, -1, 1)
    for (k in 3:D) {
      k_1[k]         <- k_1[k - 1] + factor[k - 2]*(D + 1 - k)
    }
    sequences        <- rbind(k_1, matrix(rep(0, D*(D - 1)), ncol = D))
    for (k in 2:D) {
      sequences[k, ] <- (sequences[k - 1, ] + 1)%%D
    }
    if (D%%2 == 1) {
      sequences      <- rbind(sequences, t(apply(sequences, 1, rev)))
    }
  }
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  sequences          <- convert_labels(sequences, D, labels, 1:D)
  sequences          <- transform_to_xover(sequences, labels, as_matrix)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
