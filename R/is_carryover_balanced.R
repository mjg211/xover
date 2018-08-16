#' Check for carryover balance
#'
#' Checks whether a cross-over design is balanced for first order carryover
#' effects.
#'
#' \code{is_carryover_balanced()} supports the assessment of whether a
#' cross-over design of class \code{xover_seq} (see \code{sequences}) is
#' balanced for first order carryover effects (residual effects). The user is
#' also able to specify whether there is a pre-period (see \code{pre_period}).
#' That is, if there is a pre-period each subject experiences in the first
#' period the residual effect of the treatment of the last period (i.e., the
#' last period preceeds the first period).
#'
#' Ultimately, a cross-over design is said to be balanced for first order
#' carryover effects if each treatment is preceeded by all other treatments
#' equally often, and if no treatment is preceeded by itself.
#'
#' Note that a cross-over design can be converted to be of class
#' \code{xover_seq} using \code{\link[xover]{as_xover_seq}}.
#'
#' @param sequences A cross-over design; an object of class \code{xover_seq}.
#' @param pre_period A \code{\link[base]{logical}} variable indicating whether a
#' pre-period should be assumed. See \strong{Details} for further information.
#' Defaults to \code{F}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[base]{logical}} variable indicating whether the design
#' is balanced for first order carryover effects.
#' @examples
#' # Check a three-treatment Latin square for carryover balance
#' latin    <- seq_latin(D = 3)
#' is_carryover_balanced(latin)
#' # Check a three-treatment Williams design for carryover balance
#' williams <- seq_williams(D = 3)
#' is_carryover_balanced(williams)
#' @author Based on code from the \code{\link[crossdes]{crossdes}} package by
#' Oliver Sailer.
#' @seealso \code{\link[xover]{as_xover_seq}} for converting cross-over designs
#' to class \code{xover_seq}.
#' @export
is_carryover_balanced <- function(sequences, pre_period = F, summary = T) {

  ##### Error checking #########################################################

  check_sequences(sequences)
  check_logical(pre_period, "pre_period")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the required calculations...")
  }
  if (tibble::is_tibble(sequences)) {
    sequences <- tibble_to_matrix(sequences)
  }
  J           <- ncol(sequences)
  K           <- nrow(sequences)
  labels      <- unique(as.vector(sequences))
  D           <- length(labels)
  balance     <- F
  V           <- matrix(0, D, D)
  if (pre_period) {
    V         <- diag(J)[c(J, 1:(J - 1)), ]
  } else {
    V         <- rbind(numeric(J), diag(J)[1:(J - 1), ])
  }
  X           <- matrix(0, J*K, D)
  for (k in 1:K) {
    for (j in 1:J) {
      X[(k - 1)*J + j, which(labels == sequences[k, j])] <- 1
    }
  }
  M           <- t(X) %*% kronecker(diag(K), t(V)) %*% X
  if (summary) {
    message("...completed the required calculations. Preparing outputs...")
  }
  if (all(!any(as.logical(diag(M))),
          length(unique(as.vector(M -  2*J*K*diag(D)))) == 2)) {
    balance   <- T
  } else {
    balance   <- F
  }

  ##### Ouputting ##############################################################

  if (summary) {
    message("...outputting.")
  }
  return(balance)

}
