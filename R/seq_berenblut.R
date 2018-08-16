#' Berenblut (1964) cross-over design specification
#'
#' Specifies cross-over designs from Berenblut (1964).
#'
#' \code{seq_berenblut()} supports the specification of cross-over designs from
#' Berenblut (1964). Designs for three, four, and five treatments (see \code{D})
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
#' # Berenblut (1964) design for three treatments
#' berenblut        <- seq_berenblut()
#' # Using different labels
#' berenblut_ABC    <- seq_berenblut(labels = LETTERS[1:3])
#' # Returning in tibble form
#' berenblut_tibble <- seq_berenblut(as_matrix = F)
#' @references Berenblut II (1964) Change-over designs with complete balance for
#' residual effects. \emph{Biometrics} \strong{23:}578-580.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_berenblut <- function(D = 3, labels = 0:(D - 1), as_matrix = T,
                          summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(2, 6), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (D == 3) {
    sequences <- matrix(c(1, 3, 2, 2, 3, 1, 2, 1, 3, 3, 1, 2, 3, 2, 1, 1, 2, 3,
                          1, 2, 2, 1, 3, 3, 2, 3, 3, 2, 1, 1, 3, 1, 1, 3, 2, 2,
                          1, 1, 2, 3, 3, 2, 2, 2, 3, 1, 1, 3, 3, 3, 1, 2, 2, 1),
                        9, 6, byrow = T)
  } else if (D == 4) {
    sequences <- matrix(c(1, 4, 3, 2, 2, 3, 4, 1, 2, 1, 4, 3, 3, 4, 1, 2, 3, 2,
                          1, 4, 4, 1, 2, 3, 4, 3, 2, 1, 1, 2, 3, 4, 4, 4, 2, 2,
                          1, 3, 3, 1, 1, 1, 3, 3, 2, 4, 4, 2, 2, 2, 4, 4, 3, 1,
                          1, 3, 3, 3, 1, 1, 4, 2, 2, 4, 3, 4, 1, 2, 4, 3, 2, 1,
                          4, 1, 2, 3, 1, 4, 3, 2, 1, 2, 3, 4, 2, 1, 4, 3, 2, 3,
                          4, 1, 3, 2, 1, 4, 2, 4, 4, 2, 3, 3, 1, 1, 3, 1, 1, 3,
                          4, 4, 2, 2, 4, 2, 2, 4, 1, 1, 3, 3, 1, 3, 3, 1, 2, 2,
                          4, 4), 16, 8, byrow = T)
  } else {
    sequences <- matrix(c(1, 5, 4, 3, 2, 2, 3, 4, 5, 1, 2, 1, 5, 4, 3, 3, 4, 5,
                          1, 2, 3, 2, 1, 5, 4, 4, 5, 1, 2, 3, 4, 3, 2, 1, 5, 5,
                          1, 2, 3, 4, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 1, 4, 4, 2,
                          2, 1, 3, 3, 5, 5, 2, 5, 5, 3, 3, 2, 4, 4, 1, 1, 3, 1,
                          1, 4, 4, 3, 5, 5, 2, 2, 4, 2, 2, 5, 5, 4, 1, 1, 3, 3,
                          5, 3, 3, 1, 1, 5, 2, 2, 4, 4, 1, 3, 4, 1, 2, 5, 3, 2,
                          5, 4, 2, 4, 5, 2, 3, 1, 4, 3, 1, 5, 3, 5, 1, 3, 4, 2,
                          5, 4, 2, 1, 4, 1, 2, 4, 5, 3, 1, 5, 3, 2, 5, 2, 3, 5,
                          1, 4, 2, 1, 4, 3, 1, 2, 4, 5, 2, 4, 3, 1, 5, 3, 2, 3,
                          5, 1, 3, 5, 4, 2, 1, 4, 3, 4, 1, 2, 4, 1, 5, 3, 2, 5,
                          4, 5, 2, 3, 5, 2, 1, 4, 3, 1, 5, 1, 3, 4, 1, 3, 2, 5,
                          4, 2, 1, 1, 4, 4, 2, 3, 3, 5, 5, 2, 2, 2, 5, 5, 3, 4,
                          4, 1, 1, 3, 3, 3, 1, 1, 4, 5, 5, 2, 2, 4, 4, 4, 2, 2,
                          5, 1, 1, 3, 3, 5, 5, 5, 3, 3, 1, 2, 2, 4, 4, 1),
                        25, 10, byrow = T)
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
