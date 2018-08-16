#' Carryover balanced cross-over design specification
#'
#' Specifies carryover balanced cross-over designs.
#'
#' \code{seq_williams_bib()} supports the specification of carryover balanced
#' generalized Youden designs, based on any input balanced incomplete block
#' (BIB) design (see \code{sequences}). In addition, the designs can be returned
#' in \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form (see
#' \code{as_matrix}).
#'
#' Precisely, Patterson (1951) combined BIB designs with Williams designs to get
#' carryover balanced generalized Youden designs. For each sequence in the input
#' BIB design, a Williams design is constructed using the treatments in that
#' sequence. The sequences of the resulting Williams designs are then combined.
#' Ultimately, the \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix corresponds to the treatment a
#' subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' Note that the resulting design will only be properly balanced if the input
#' design is actually a BIB design. A warning message will be printed if this is
#' not the case.
#'
#' BIB designs can be generated using \code{\link[xover]{seq_bib}}.
#'
#' @param sequences A balanced incomplete block design, of class
#' \code{xover_seq}.
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
#' # Find a BIB design
#' bib                 <- seq_bib()
#' # Use this to construct a carryover balanced design
#' williams_bib        <- seq_williams_bib(bib)
#' # Returning in tibble form
#' williams_bib_tibble <- seq_williams_bib(bib, as_matrix = F)
#' @references Patterson HD (1951) Change-over trials. \emph{J R Stat Soc B}
#' \strong{13:}256-271.
#' @references Wakeling IN, MacFie HJH (1995) Designing consumer trials balanced
#' for first and higher orders of carry-over effect when only a subset of k
#' samples from t may be tested. \emph{Food Qual Prefer} \strong{6:}299-308.
#' @references Williams EJ (1949) Experimental designs balanced for the
#' estimation of residual effects of treatments. \emph{Aust J Sci Res Ser A}
#' \strong{2:}149-168.
#' @author Based on code from the \code{\link[crossdes]{crossdes}} package by
#' Oliver Sailer.
#' @seealso \code{\link[crossdes]{seq_bib}},
#' \code{\link[crossdes]{seq_williams}}.
#' @export
seq_williams_bib <- function(sequences, as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_sequences(sequences)
  classify <- classify_seq(sequences)
  if (classify != "balanced incomplete block design w.r.t. rows") {
    warning("Input sequences are not a balanced incomplete block design")
  }
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (tibble::is_tibble(sequences)) {
    sequences     <- tibble_to_matrix(sequences)
  }
  J               <- ncol(sequences)
  K               <- nrow(sequences)
  labels          <- unique(as.vector(sequences))
  williams        <- seq_williams(J, 1:J, T, F)
  if (!(J%%2)) {
    new_sequences <- matrix(0, J*K, J)
  } else {
    new_sequences <- matrix(0, 2*J*K, K)
  }
  if (!(J%%2)) {
    for (k in 1:K) {
      new_sequences[((k - 1)*J + 1):((k - 1)*J + J), ]       <-
        matrix(sequences[k, williams], J, J)
    }
  } else {
    for (k in 1:K) {
      new_sequences[((k - 1)*2*J + 1):((k - 1)*2*J + 2*J), ] <-
        matrix(sequences[k, williams], 2*J, J)
    }
  }
  sequences       <- new_sequences
  K               <- nrow(sequences)
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  sequences       <- transform_to_xover(sequences, labels, as_matrix)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
