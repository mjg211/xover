#' Balaam (1968) cross-over design specification
#'
#' Specifies cross-over designs from Balaam (1968).
#'
#' \code{seq_balaam()} supports the specification of cross-over designs from
#' Balaam (1968). Designs for three through six treatments (see \code{D}) are
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
#' \code{\link[base]{numeric}} integer greater than or equal to two. Defaults to
#' \code{3}.
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
#' # Balaam (1968) design for three treatments
#' balaam        <- seq_balaam()
#' # Using different labels
#' balaam_ABC    <- seq_balaam(labels = LETTERS[1:3])
#' # Returning in tibble form
#' balaam_tibble <- seq_balaam(as_matrix = F)
#' @references Balaam LN (1968) A two-period design with \emph{t}² experimental
#' units. \emph{Biometrics} \strong{24:}61-73.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_balaam <- function(D = 3, labels = 0:(D - 1), as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(2, 7), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (D == 3) {
    sequences <- matrix(c(1, 1, 2, 2, 3, 3, 1, 2, 2, 3, 3, 1, 1, 3, 2, 1, 3, 2),
                        9, 2, byrow = T)
  } else if (D == 4) {
    sequences <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 1, 2, 2, 3, 3, 4, 4, 1, 1, 3,
                          2, 4, 3, 1, 4, 2, 1, 4, 2, 1, 3, 2, 4, 3), 16, 2,
                        byrow = T)
  } else if (D == 5) {
    sequences <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 1, 2, 2, 3, 3, 4, 4, 5,
                          5, 1, 1, 3, 2, 4, 3, 5, 4, 1, 5, 2, 1, 4, 2, 5, 3, 1,
                          4, 2, 5, 3, 1, 5, 2, 1, 3, 2, 4, 3, 5, 4), 25, 2,
                        byrow = T)
  } else {
    sequences <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 1, 2, 2, 3, 3, 4,
                          4, 5, 5, 6, 6, 1, 1, 3, 2, 4, 3, 5, 4, 6, 5, 1, 6, 2,
                          1, 4, 2, 5, 3, 6, 4, 1, 5, 2, 6, 3, 1, 5, 2, 6, 3, 1,
                          4, 2, 5, 3, 6, 4, 1, 6, 2, 1, 3, 2, 4, 3, 5, 4, 6, 5),
                        36, 2, byrow = T)
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
