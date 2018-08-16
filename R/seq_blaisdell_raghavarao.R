#' Blaisdell and Raghavarao (1980) cross-over design specification
#'
#' Specifies cross-over designs from Blaisdell and Raghavarao (1980).
#'
#' \code{seq_blaisdell_raghavarao()} supports the specification of designs
#' from Blaisdell and Raghavarao (1980). Designs for six, eight, and nine
#' treatments (see \code{D}) are supported, for any chosen treatment labels (see
#' \code{labels}). In addition, the designs can be returned in
#' \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form (see
#' \code{as_matrix}).
#'
#' Precisely, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' cross-over design matrix corresponds to the treatment a subject on the
#' \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be either six, eight, or nine.
#' Defaults to \code{6}.
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
#' # Blaisdell and Raghavarao (1980) design for six treatments
#' blaisdell_raghavarao        <- seq_blaisdell_raghavarao()
#' # Using different labels
#' blaisdell_raghavarao_ABCDEF <- seq_blaisdell_raghavarao(labels =
#'                                                           LETTERS[1:6])
#' # Returning in tibble form
#' blaisdell_raghavarao_tibble <- seq_blaisdell_raghavarao(as_matrix = F)
#' @references Blaisdell EA, Raghavarao D (1980) Partially balanced change-over
#' designs based on m-associate class PBIB designs. \emph{J R Stat Soc B}
#' \strong{42:}334-8.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_blaisdell_raghavarao <- function(D = 6, labels = 0:(D - 1), as_matrix = T,
                                     summary = T) {

  ##### Input checking #########################################################

  check_belong(D, "D", c(6, 8, 9), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (D == 6) {
    sequences <- matrix(c(1, 2, 3, 2, 3, 1, 3, 1, 2, 3, 2, 1, 1, 3, 2, 2, 1, 3,
                          1, 4, 5, 4, 5, 1, 5, 1, 4, 5, 4, 1, 1, 5, 4, 4, 1, 5,
                          2, 4, 6, 4, 6, 2, 6, 2, 4, 6, 4, 2, 2, 6, 4, 4, 2, 6,
                          3, 5, 6, 5, 6, 3, 6, 3, 5, 6, 5, 3, 3, 6, 5, 5, 3, 6),
                        24, 3, byrow = T)
  } else if (D == 8) {
    sequences <- matrix(c(1, 2, 4, 3, 7, 2, 3, 1, 4, 8, 3, 4, 2, 1, 5, 4, 1, 3,
                          2, 6, 5, 6, 8, 7, 3, 6, 7, 5, 8, 4, 7, 8, 6, 5, 1, 8,
                          5, 7, 6, 2), 8, 5, byrow = T)
  } else {
    sequences <- matrix(c(1, 2, 3, 6, 2, 3, 1, 4, 3, 1, 2, 5, 3, 2, 1, 7, 1, 3,
                          2, 8, 2, 1, 3, 9, 4, 5, 6, 3, 5, 6, 4, 1, 6, 4, 5, 2,
                          6, 5, 4, 7, 4, 6, 5, 8, 5, 4, 6, 9, 7, 8, 9, 3, 8, 9,
                          7, 1, 9, 7, 8, 2, 9, 8, 7, 4, 7, 9, 8, 5, 8, 7, 9, 6),
                        18, 4, byrow = T)
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
