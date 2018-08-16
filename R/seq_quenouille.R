#' Quenouille (1953) cross-over design specification
#'
#' Specifies cross-over designs from Quenouille (1953).
#'
#' \code{seq_quenouille()} supports the specification of designs from
#' Quenouille (1953). Designs for three or four treatments (see \code{D}) are
#' supported, for any chosen treatment labels (see \code{labels}). In addition,
#' the designs can be returned in \code{\link[base]{matrix}} or
#' \code{\link[tibble]{tibble}} form (see \code{as_matrix}).
#'
#' Precisely, for \code{D = 3}, two designs are available (accessible by setting
#' \code{selection} equal to one or two), and for \code{D = 4}, three designs
#' are available (accessible by setting \code{selection} equal to one, two, or
#' three). Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' cross-over design matrix corresponds to the treatment a subject on the
#' ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be either three or four. Defaults to
#' \code{3}.
#' @param selection A single \code{\link[base]{numeric}} integer indicating
#' which design to return, for the chosen value of \code{D}. See
#' \strong{Details} for information on supported values.
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
#' # Quenouille (1953) designs for three treatments
#' quenouille1        <- seq_quenouille()
#' quenouille2        <- seq_quenouille(selection = 2)
#' # Using different labels
#' quenouille1_ABC    <- seq_quenouille(labels = LETTERS[1:3])
#' quenouille2_ABC    <- seq_quenouille(selection = 2, labels = LETTERS[1:3])
#' # Returning in tibble form
#' quenouille1_tibble <- seq_quenouille(as_matrix = F)
#' quenouille2_tibble <- seq_quenouille(selection = 2, as_matrix = F)
#' @references Quenouille MH (1953) \emph{The Design and Analysis of
#' Experiments}. Griffin: London.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_quenouille <- function(D = 3, selection = 1, labels = 0:(D - 1),
                           as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_belong(D, "D", c(3, 4), 1)
  check_selection(selection, c(rep(NA, 2), 2, 3), D)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (all(D == 3, selection == 1)) {
    sequences <- matrix(c(1, 1, 2, 2, 3, 3, 1, 2, 2, 3, 1, 3, 2, 2, 3, 3, 1, 1,
                          2, 3, 3, 1, 2, 1, 3, 3, 1, 1, 2, 2, 3, 1, 1, 2, 3, 2,
                          1, 1, 3, 3, 2, 2, 1, 3, 3, 2, 1, 2, 3, 3, 2, 2, 1, 1,
                          3, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 2, 1, 1, 3, 2, 3,
                          1, 2, 3, 2, 3, 1, 2, 3, 2, 1, 1, 3, 3, 2, 1, 3, 2, 1,
                          2, 1, 3, 1, 3, 2, 1, 3, 1, 2, 2, 3, 3, 1, 2, 3, 1, 2),
                        18, 6, byrow = T)
  } else if (all(D == 3, selection == 2)) {
    sequences <- matrix(c(1, 1, 2, 2, 3, 3, 1, 2, 2, 3, 1, 3, 2, 2, 3, 3, 1, 1,
                          2, 3, 3, 1, 2, 1, 3, 3, 1, 1, 2, 2, 3, 1, 1, 2, 3, 2,
                          1, 1, 3, 3, 2, 2, 1, 3, 3, 2, 1, 2, 3, 3, 2, 2, 1, 1,
                          3, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 2, 1, 1, 3, 2, 3,
                          1, 2, 3, 2, 3, 1, 2, 3, 2, 1, 1, 3, 3, 2, 1, 3, 2, 1,
                          2, 1, 3, 1, 3, 2, 1, 3, 1, 2, 2, 3, 3, 1, 2, 3, 1, 2),
                        18, 6, byrow = T)
  } else if (all(D == 4, selection == 1)) {
    sequences <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 1, 2, 2, 3, 3, 4, 4, 1, 2, 2,
                          3, 3, 4, 4, 1, 1, 2, 3, 3, 4, 4, 1, 1, 2, 3, 3, 4, 4,
                          1, 1, 2, 2, 3, 4, 4, 1, 1, 2, 2, 3, 4, 4, 1, 1, 2, 2,
                          3, 3, 4, 1, 1, 2, 2, 3, 3, 4, 1, 3, 2, 1, 4, 2, 4, 3,
                          3, 2, 1, 4, 2, 4, 3, 1, 2, 1, 4, 2, 4, 3, 1, 3, 1, 4,
                          2, 4, 3, 1, 3, 2, 4, 2, 4, 3, 1, 3, 2, 1, 2, 4, 3, 1,
                          3, 2, 1, 4, 4, 3, 1, 3, 2, 1, 4, 2, 3, 1, 3, 2, 1, 4,
                          2, 4), 16, 8, byrow = T)
  } else if (all(D == 4, selection == 2)) {
    sequences <- matrix(c(1, 1, 2, 2, 3, 4, 4, 3, 1, 2, 2, 3, 4, 4, 3, 1, 2, 2,
                          3, 4, 4, 3, 1, 1, 2, 3, 4, 4, 3, 1, 1, 2, 3, 4, 4, 3,
                          1, 1, 2, 2, 4, 4, 3, 1, 1, 2, 2, 3, 4, 3, 1, 1, 2, 2,
                          3, 4, 3, 1, 1, 2, 2, 3, 4, 4, 1, 3, 3, 2, 4, 1, 4, 2,
                          3, 3, 2, 4, 1, 4, 2, 1, 3, 2, 4, 1, 4, 2, 1, 3, 2, 4,
                          1, 4, 2, 1, 3, 3, 4, 1, 4, 2, 1, 3, 3, 2, 1, 4, 2, 1,
                          3, 3, 2, 4, 4, 2, 1, 3, 3, 2, 4, 1, 2, 1, 3, 3, 2, 4,
                          1, 4), 16, 8, byrow = T)
  } else {
    sequences <- matrix(c(1, 1, 2, 3, 4, 4, 3, 2, 1, 2, 3, 4, 4, 3, 2, 1, 2, 3,
                          4, 4, 3, 2, 1, 1, 3, 4, 4, 3, 2, 1, 1, 2, 4, 4, 3, 2,
                          1, 1, 2, 3, 4, 3, 2, 1, 1, 2, 3, 4, 3, 2, 1, 1, 2, 3,
                          4, 4, 2, 1, 1, 2, 3, 4, 4, 3, 3, 3, 1, 4, 2, 2, 4, 1,
                          3, 1, 4, 2, 2, 4, 1, 3, 1, 4, 2, 2, 4, 1, 3, 3, 4, 2,
                          2, 4, 1, 3, 3, 1, 2, 2, 4, 1, 3, 3, 1, 4, 2, 4, 1, 3,
                          3, 1, 4, 2, 4, 1, 3, 3, 1, 4, 2, 2, 1, 3, 3, 1, 4, 2,
                          2, 4), 16, 8, byrow = T)
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
