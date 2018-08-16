#' Bate and Jones (2002) cross-over design specification
#'
#' Specifies cross-over designs from Bate and Jones (2002).
#'
#' \code{seq_bate_jones()} supports the specification of cross-over designs from
#' Bate and Jones (2002). Designs for five and eight treatments (see \code{D})
#' are supported, for any chosen treatment labels (see \code{labels}). In
#' addition, the designs can be returned in \code{\link[base]{matrix}} or
#' \code{\link[tibble]{tibble}} form (see \code{as_matrix}).
#'
#' Precisely, the \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix corresponds to the treatment a
#' subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be either five or eight. Defaults to
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
#' # Bate and Jones (2002) design for five treatments
#' bate_jones        <- seq_bate_jones()
#' # Using different labels
#' bate_jones_ABCDE  <- seq_bate_jones(labels = LETTERS[1:5])
#' # Returning in tibble form
#' bate_jones_tibble <- seq_bate_jones(as_matrix = F)
#' @references Bate S, Jones B (2002) The construction of universally optimal
#' uniform cross-over designs. \emph{GlaxoSmithKline Biomedical Data Sciences
#' Technical Report}.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_bate_jones <- function(D = 5, labels = 0:(D - 1), as_matrix = T,
                           summary = T) {

  ##### Input checking #########################################################

  check_belong(D, "D", c(5, 8), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (D == 5) {
    sequences <- matrix(c(1, 3, 2, 5, 4, 4, 5, 2, 3, 1, 2, 4, 3, 1, 5, 5, 1, 3,
                          4, 2, 3, 5, 4, 2, 1, 1, 2, 4, 5, 3, 4, 1, 5, 3, 2, 2,
                          3, 5, 1, 4, 5, 2, 1, 4, 3, 3, 4, 1, 2, 5, 3, 2, 5, 4,
                          4, 5, 2, 3, 1, 1, 4, 3, 1, 5, 5, 1, 3, 4, 2, 2, 5, 4,
                          2, 1, 1, 2, 4, 5, 3, 3, 1, 5, 3, 2, 2, 3, 5, 1, 4, 4,
                          2, 1, 4, 3, 3, 4, 1, 2, 5, 5, 2, 5, 4, 4, 5, 2, 3, 1,
                          1, 3, 3, 1, 5, 5, 1, 3, 4, 2, 2, 4, 4, 2, 1, 1, 2, 4,
                          5, 3, 3, 5, 5, 3, 2, 2, 3, 5, 1, 4, 4, 1, 1, 4, 3, 3,
                          4, 1, 2, 5, 5, 2), 15, 10, byrow = T)
  } else {
    sequences <- matrix(c(1, 2, 8, 3, 7, 4, 6, 5, 5, 6, 4, 7, 3, 8, 2, 1, 2, 3,
                          1, 4, 8, 5, 7, 6, 6, 7, 5, 8, 4, 1, 3, 2, 3, 4, 2, 5,
                          1, 6, 8, 7, 7, 8, 6, 1, 5, 2, 4, 3, 4, 5, 3, 6, 2, 7,
                          1, 8, 8, 1, 7, 2, 6, 3, 5, 4, 5, 6, 4, 7, 3, 8, 2, 1,
                          1, 2, 8, 3, 7, 4, 6, 5, 6, 7, 5, 8, 4, 1, 3, 2, 2, 3,
                          1, 4, 8, 5, 7, 6, 7, 8, 6, 1, 5, 2, 4, 3, 3, 4, 2, 5,
                          1, 6, 8, 7, 8, 1, 7, 2, 6, 3, 5, 4, 4, 5, 3, 6, 2, 7,
                          1, 8, 2, 8, 3, 7, 4, 6, 5, 5, 6, 4, 7, 3, 8, 2, 1, 1,
                          3, 1, 4, 8, 5, 7, 6, 6, 7, 5, 8, 4, 1, 3, 2, 2, 4, 2,
                          5, 1, 6, 8, 7, 7, 8, 6, 1, 5, 2, 4, 3, 3, 5, 3, 6, 2,
                          7, 1, 8, 8, 1, 7, 2, 6, 3, 5, 4, 4, 6, 4, 7, 3, 8, 2,
                          1, 1, 2, 8, 3, 7, 4, 6, 5, 5, 7, 5, 8, 4, 1, 3, 2, 2,
                          3, 1, 4, 8, 5, 7, 6, 6, 8, 6, 1, 5, 2, 4, 3, 3, 4, 2,
                          5, 1, 6, 8, 7, 7, 1, 7, 2, 6, 3, 5, 4, 4, 5, 3, 6, 2,
                          7, 1, 8, 8), 16, 16, byrow = T)
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
