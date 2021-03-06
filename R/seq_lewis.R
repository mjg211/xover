#' Lewis \emph{et al.} (1988) cross-over design specification
#'
#' Specifies cross-over designs from Lewis \emph{et al.} (1988).
#'
#' \code{seq_lewis()} supports the specification of designs from Lewis
#' \emph{et al.} (1988). Designs for four, six, eight, and nine treatments (see
#' \code{D}) are supported, for any chosen treatment labels (see \code{labels}).
#' In addition, the designs can be returned in \code{\link[base]{matrix}} or
#' \code{\link[tibble]{tibble}} form (see \code{as_matrix}).
#'
#' Precisely, for \code{D} equal to four, six, eight and nine, there are six,
#' eight, four, and two sets of sequences available respectively (accessible by
#' setting \code{selection} equal to one through eight as appropriate).
#' Ultimately, the \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix corresponds to the treatment a
#' subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be either four, six, eight, or nine.
#' Defaults to \code{4}.
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
#' # Lewis et al. (1988) designs for nine treatments
#' lewis1         <- seq_lewis(D = 9)
#' lewis2         <- seq_lewis(D = 9, selection = 2)
#' # Using different labels
#' lewis1_LETTERS <- seq_lewis(D = 9, labels = LETTERS[1:9])
#' lewis2_LETTERS <- seq_lewis(D = 9, selection = 2, labels = LETTERS[1:9])
#' # Returning in tibble form
#' lewis1_tibble  <- seq_lewis(D = 9, as_matrix = F)
#' lewis2_tibble  <- seq_lewis(D = 9, selection = 2, as_matrix = F)
#' @references Lewis SM, Fletcher DJ, Matthews JNS (1988) Factorial cross-over
#' designs in clinical trials. In \emph{Optimal Design and Analysis of
#' Experiments}, Eds: Dodge Y, Fedorov VV, Wynn HP, pp 133-140. Elsevier Science
#' Publishers BV: North-Holland.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_lewis <- function(D = 4, selection = 1, labels = 0:(D - 1), as_matrix = T,
                      summary = T) {

  ##### Input checking #########################################################

  check_belong(D, "D", c(4, 6, 8, 9), 1)
  check_selection(selection, c(NA, NA, NA, 6, NA, 8, NA, 4, 2), D)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (all(D == 4, selection == 1)) {
    sequences <- matrix(c(1, 3, 2, 2, 4, 1, 3, 1, 4, 4, 2, 3, 1, 3, 4, 2, 4, 3,
                          3, 1, 2, 4, 2, 1), 8, 3, byrow = T)
  } else if (all(D == 4, selection == 2)) {
    sequences <- matrix(c(1, 4, 2, 2, 3, 1, 3, 2, 4, 4, 1, 3, 1, 4, 3, 2, 3, 4,
                          3, 2, 1, 4, 1, 2), 8, 3, byrow = T)
  } else if (all(D == 4, selection == 3)) {
    sequences <- matrix(c(1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4, 3, 2, 1, 1, 3,
                          2, 4, 2, 4, 1, 3, 3, 1, 4, 2, 4, 2, 3, 1), 8, 4,
                        byrow = T)
  } else if (all(D == 4, selection == 4)) {
    sequences <- matrix(c(1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4, 3, 2, 1, 1, 3,
                          4, 2, 2, 4, 3, 1, 3, 1, 2, 4, 4, 2, 1, 3), 8, 4,
                        byrow = T)
  } else if (all(D == 4, selection == 5)) {
    sequences <- matrix(c(1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4, 3, 2, 1, 1, 4,
                          2, 3, 2, 3, 1, 4, 3, 2, 4, 1, 4, 1, 3, 2), 8, 4,
                        byrow = T)
  } else if (all(D == 4, selection == 6)) {
    sequences <- matrix(c(1, 2, 4, 3, 2, 1, 3, 4, 3, 4, 2, 1, 4, 3, 1, 2, 1, 4,
                          3, 2, 2, 3, 4, 1, 3, 2, 1, 4, 4, 1, 2, 3), 8, 4,
                        byrow = T)
  } else if (all(D == 6, selection == 1)) {
    sequences <- matrix(c(1, 2, 4, 2, 3, 5, 3, 1, 6, 4, 5, 1, 5, 6, 2, 6, 4, 3,
                          1, 3, 6, 2, 1, 4, 3, 2, 5, 4, 6, 3, 5, 4, 1, 6, 5, 2,
                          1, 2, 5, 2, 3, 6, 3, 1, 4, 4, 5, 2, 5, 6, 3, 6, 4, 1,
                          1, 3, 4, 2, 1, 5, 3, 2, 6, 4, 6, 1, 5, 4, 2, 6, 5, 3),
                        24, 3, byrow = T)
  } else if (all(D == 6, selection == 2)) {
    sequences <- matrix(c(1, 2, 5, 2, 3, 6, 3, 1, 4, 4, 5, 2, 5, 6, 3, 6, 4, 1,
                          1, 5, 4, 2, 6, 5, 3, 4, 6, 4, 2, 1, 5, 3, 2, 6, 1, 3,
                          1, 3, 6, 2, 1, 4, 3, 2, 5, 4, 6, 3, 5, 4, 1, 6, 5, 2,
                          1, 6, 4, 2, 4, 5, 3, 5, 6, 4, 3, 1, 5, 1, 2, 6, 2, 3),
                        24, 3, byrow = T)
  } else if (all(D == 6, selection == 3)) {
    sequences <- matrix(c(1, 4, 2, 2, 5, 3, 3, 6, 1, 4, 1, 5, 5, 2, 6, 6, 3, 4,
                          1, 4, 5, 2, 5, 6, 3, 6, 4, 4, 1, 2, 5, 2, 3, 6, 3, 1),
                        12, 3, byrow = T)
  } else if (all(D == 6, selection == 4)) {
    sequences <- matrix(c(1, 4, 5, 2, 5, 6, 3, 6, 4, 4, 1, 2, 5, 2, 3, 6, 3, 1,
                          1, 4, 6, 2, 5, 4, 3, 6, 5, 4, 1, 3, 5, 2, 1, 6, 3, 2),
                        12, 3, byrow = T)
  } else if (all(D == 6, selection == 5)) {
    sequences <- matrix(c(1, 4, 5, 2, 5, 6, 3, 6, 4, 4, 1, 2, 5, 2, 3, 6, 3, 1,
                          1, 5, 2, 2, 6, 3, 3, 4, 1, 4, 2, 5, 5, 3, 6, 6, 1, 4,
                          1, 4, 6, 2, 5, 4, 3, 6, 5, 4, 1, 3, 5, 2, 1, 6, 3, 2,
                          1, 6, 3, 2, 4, 1, 3, 5, 2, 4, 3, 6, 5, 1, 4, 6, 2, 5),
                        24, 3, byrow = T)
  } else if (all(D == 6, selection == 6)) {
    sequences <- matrix(c(1, 5, 2, 2, 6, 3, 3, 4, 1, 4, 2, 5, 5, 3, 6, 6, 1, 4,
                          1, 6, 4, 2, 4, 5, 3, 5, 6, 4, 3, 1, 5, 1, 2, 6, 2, 3,
                          1, 5, 4, 2, 6, 5, 3, 4, 6, 4, 2, 1, 5, 3, 2, 6, 1, 3,
                          1, 6, 3, 2, 4, 1, 3, 5, 2, 4, 3, 6, 5, 1, 4, 6, 2, 5),
                        24, 3, byrow = T)
  } else if (all(D == 6, selection == 7)) {
    sequences <- matrix(c(1, 2, 5, 3, 2, 3, 6, 1, 3, 1, 4, 2, 4, 5, 2, 6, 5, 6,
                          3, 4, 6, 4, 1, 5), 6, 4, byrow = T)
  } else if (all(D == 6, selection == 8)) {
    sequences <- matrix(c(1, 5, 6, 2, 2, 6, 4, 3, 3, 4, 5, 1, 4, 2, 3, 5, 5, 3,
                          1, 6, 6, 1, 2, 4), 6, 4, byrow = T)
  } else if (all(D == 8, selection == 1)) {
    sequences <- matrix(c(1, 3, 2, 6, 2, 4, 3, 7, 3, 1, 4, 8, 4, 2, 1, 5, 5, 7,
                          6, 2, 6, 8, 7, 3, 7, 5, 8, 4, 8, 6, 5, 1, 1, 3, 4, 8,
                          2, 4, 1, 5, 3, 1, 2, 6, 4, 2, 3, 7, 5, 7, 8, 4, 6, 8,
                          5, 1, 7, 5, 6, 2, 8, 6, 7, 3), 16, 4, byrow = T)
  } else if (all(D == 8, selection == 2)) {
    sequences <- matrix(c(1, 3, 2, 7, 2, 4, 3, 8, 3, 1, 4, 5, 4, 2, 1, 6, 5, 7,
                          6, 3, 6, 8, 7, 4, 7, 5, 8, 1, 8, 6, 5, 2, 1, 3, 4, 7,
                          2, 4, 1, 8, 3, 1, 2, 5, 4, 2, 3, 6, 5, 7, 8, 3, 6, 8,
                          5, 4, 7, 5, 6, 1, 8, 6, 7, 2), 16, 4, byrow = T)
  } else if (all(D == 8, selection == 3)) {
    sequences <- matrix(c(1, 3, 6, 2, 2, 4, 7, 3, 3, 1, 8, 4, 4, 2, 5, 1, 5, 7,
                          2, 6, 6, 8, 3, 7, 7, 5, 4, 8, 8, 6, 1, 5, 1, 3, 8, 4,
                          2, 4, 5, 1, 3, 1, 6, 2, 4, 2, 7, 3, 5, 7, 4, 8, 6, 8,
                          1, 5, 7, 5, 2, 6, 8, 6, 3, 7), 16, 4, byrow = T)
  } else if (all(D == 8, selection == 4)) {
    sequences <- matrix(c(1, 3, 6, 7, 2, 4, 7, 8, 3, 1, 8, 5, 4, 2, 5, 6, 5, 7,
                          2, 3, 6, 8, 3, 4, 7, 5, 4, 1, 8, 6, 1, 2, 1, 3, 8, 7,
                          2, 4, 5, 8, 3, 1, 6, 5, 4, 2, 7, 6, 5, 7, 4, 3, 6, 8,
                          1, 4, 7, 5, 2, 1, 8, 6, 3, 2), 16, 4, byrow = T)
  } else if (all(D == 9, selection == 1)) {
    sequences <- matrix(c(1, 4, 8, 1, 4, 9, 2, 5, 9, 2, 5, 7, 3, 6, 7, 3, 6, 8,
                          4, 7, 2, 4, 7, 3, 5, 8, 3, 5, 8, 1, 6, 9, 1, 6, 9, 2,
                          7, 1, 5, 7, 1, 6, 8, 2, 6, 8, 2, 4, 9, 3, 4, 9, 3, 5,
                          1, 7, 5, 1, 7, 6, 2, 8, 6, 2, 8, 4, 3, 9, 4, 3, 9, 5,
                          4, 1, 8, 4, 1, 9, 5, 2, 9, 5, 2, 7, 6, 3, 7, 6, 3, 8,
                          7, 4, 2, 7, 4, 3, 8, 5, 3, 8, 5, 1, 9, 6, 1, 9, 6, 2),
                        36, 3, byrow = T)
  } else if (all(D == 9, selection == 2)) {
    sequences <- matrix(c(1, 5, 7, 6, 1, 8, 4, 9, 2, 6, 8, 4, 2, 9, 5, 7, 3, 4,
                          9, 5, 3, 7, 6, 8, 4, 8, 1, 9, 4, 2, 7, 3, 5, 9, 2, 7,
                          5, 3, 8, 1, 6, 7, 3, 8, 6, 1, 9, 2, 7, 2, 4, 3, 7, 5,
                          1, 6, 8, 3, 5, 1, 8, 6, 2, 4, 9, 1, 6, 2, 9, 4, 3, 5,
                          1, 6, 7, 5, 1, 9, 4, 8, 2, 4, 8, 6, 2, 7, 5, 9, 3, 5,
                          9, 4, 3, 8, 6, 7, 4, 9, 1, 8, 4, 3, 7, 2, 5, 7, 2, 9,
                          5, 1, 8, 3, 6, 8, 3, 7, 6, 2, 9, 1, 7, 3, 4, 2, 7, 6,
                          1, 5, 8, 1, 5, 3, 8, 4, 2, 6, 9, 2, 6, 1, 9, 5, 3, 4),
                        36, 4, byrow = T)
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
