#' Lucas (1956) cross-over design specification
#'
#' Specifies cross-over designs from Lucas (1956).
#'
#' \code{seq_lucas()} supports the specification of designs from Lucas
#' (1956). Designs for three through seven treatments (see \code{D}) are
#' supported, for any chosen treatment labels (see \code{labels}). In addition,
#' the designs can be returned in \code{\link[base]{matrix}} or
#' \code{\link[tibble]{tibble}} form (see \code{as_matrix}).
#'
#' Precisely, the \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix corresponds to the treatment a
#' subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} integer between three and seven inclusive.
#' Defaults to \code{3}.
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
#' # Lucas (1956) designs for three treatments
#' lucas        <- seq_lucas()
#' # Using different labels
#' lucas_ABC    <- seq_lucas(labels = LETTERS[1:3])
#' # Returning in tibble form
#' lucas_tibble <- seq_lucas(as_matrix = F)
#' @references Lucas HL (1956) Switch-back trials for more than two treatments.
#' \emph{J Dairy Sci} \strong{39:}146-154.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_lucas <- function(D = 3, labels = 0:(D - 1), as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(2, 8), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (D == 3) {
    sequences <- matrix(c(1, 2, 1, 2, 3, 2, 3, 1, 3, 1, 3, 1, 2, 1, 2, 3, 2, 3),
                        6, 3, byrow = T)
  } else if (D == 4) {
    sequences <- matrix(c(1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 1, 4, 1, 3, 1, 2, 4, 2,
                          3, 1, 3, 4, 2, 4, 1, 4, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4),
                        12, 3, byrow = T)
  } else if (D == 5) {
    sequences <- matrix(c(1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5, 1, 5, 1, 3, 1,
                          2, 4, 2, 3, 5, 3, 4, 1, 4, 5, 2, 5, 2, 1, 2, 3, 2, 3,
                          4, 3, 4, 5, 4, 5, 1, 5, 1, 3, 1, 3, 4, 2, 4, 5, 3, 5,
                          1, 4, 1, 2, 5, 2), 20, 3, byrow = T)
  } else if (D == 6) {
    sequences <- matrix(c(1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5, 6, 5, 6, 1, 6,
                          1, 3, 1, 2, 4, 2, 3, 5, 3, 4, 6, 4, 5, 1, 5, 6, 2, 6,
                          1, 4, 1, 2, 5, 2, 3, 6, 3, 4, 1, 4, 5, 2, 5, 6, 3, 6,
                          1, 5, 1, 2, 6, 2, 3, 1, 3, 4, 2, 4, 5, 3, 5, 6, 4, 6,
                          1, 6, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5, 6, 5, 6),
                        30, 3, byrow = T)
  } else {
    sequences <- matrix(c(1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5, 4, 5, 6, 5, 6, 7, 6,
                          7, 1, 7, 1, 3, 1, 2, 4, 2, 3, 5, 3, 4, 6, 4, 5, 7, 5,
                          6, 1, 6, 7, 2, 7, 1, 4, 1, 2, 5, 2, 3, 6, 3, 4, 7, 4,
                          5, 1, 5, 6, 2, 6, 7, 3, 7, 2, 1, 2, 3, 2, 3, 4, 3, 4,
                          5, 4, 5, 6, 5, 6, 7, 6, 7, 1, 7, 1, 3, 1, 3, 4, 2, 4,
                          5, 3, 5, 6, 4, 6, 7, 5, 7, 1, 6, 1, 2, 7, 2, 4, 1, 4,
                          5, 2, 5, 6, 3, 6, 7, 4, 7, 1, 5, 1, 2, 6, 2, 3, 7, 3),
                        42, 3, byrow = T)
  }
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  sequences   <- convert_labels(sequences, D, labels, 1:D)
  sequences   <- transform_to_xover(sequences, labels, as_matrix)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
