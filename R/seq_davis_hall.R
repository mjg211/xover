#' Davis and Hall (1969) cross-over design specification
#'
#' Specifies cross-over designs from Davis and Hall (1969).
#'
#' \code{seq_davis_hall()} supports the specification of designs from
#' Davis and Hall (1969). Designs for six through nine treatments (see
#' \code{D}) are supported, for any chosen treatment labels (see \code{labels}).
#' In addition, the designs can be returned in \code{\link[base]{matrix}} or
#' \code{\link[tibble]{tibble}} form (see \code{as_matrix}).
#'
#' Precisely, for each supported value of \code{D}, there are three designs
#' available (accessible by setting \code{selection} equal to one, two, or
#' three). Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' cross-over design matrix corresponds to the treatment a subject on the
#' \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be either six, seven, eight, or nine.
#' Defaults to \code{6}.
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
#' # Davis and Hall (1969) designs for six treatments
#' davis_hall1        <- seq_davis_hall()
#' davis_hall2        <- seq_davis_hall(selection = 2)
#' davis_hall3        <- seq_davis_hall(selection = 3)
#' # Using different labels
#' davis_hall1_ABCDEF <- seq_davis_hall(labels = LETTERS[1:6])
#' davis_hall2_ABCDEF <- seq_davis_hall(selection = 2, labels = LETTERS[1:6])
#' davis_hall3_ABCDEF <- seq_davis_hall(selection = 3, labels = LETTERS[1:6])
#' # Returning in tibble form
#' davis_hall1_tibble <- seq_davis_hall(as_matrix = F)
#' davis_hall2_tibble <- seq_davis_hall(selection = 2, as_matrix = F)
#' davis_hall3_tibble <- seq_davis_hall(selection = 3, as_matrix = F)
#' @references Davis AW, Hall WB (1969) Cyclic change-over designs.
#' \emph{Biometrika} \strong{56:}283-93.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_davis_hall <- function(D = 6, selection = 1, labels = 0:(D - 1),
                           as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(5, 10), 1)
  check_selection(selection, c(rep(NA, 5), 3, 3, 3, 3), D)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (all(D == 6, selection == 1)) {
    sequences <- matrix(c(1, 4, 5, 2, 5, 6, 3, 6, 1, 4, 1, 2, 5, 2, 3, 6, 3, 4,
                          1, 6, 2, 2, 1, 3, 3, 2, 4, 4, 3, 5, 5, 4, 6, 6, 5, 1),
                        12, 3, byrow = T)
  } else if (all(D == 6, selection == 2)) {
    sequences <- matrix(c(1, 2, 4, 3, 2, 3, 5, 4, 3, 4, 6, 5, 4, 5, 1, 6, 5, 6,
                          2, 1, 6, 1, 3, 2, 1, 4, 2, 5, 2, 5, 3, 6, 3, 6, 4, 1,
                          4, 1, 5, 2, 5, 2, 6, 3, 6, 3, 1, 4), 12, 4, byrow = T)
  } else if (all(D == 6, selection == 3)) {
    sequences <- matrix(c(1, 2, 4, 3, 6, 2, 3, 5, 4, 1, 3, 4, 6, 5, 2, 4, 5, 1,
                          6, 3, 5, 6, 2, 1, 4, 6, 1, 3, 2, 5), 6, 5, byrow = T)
  } else if (all(D == 7, selection == 1)) {
    sequences <- matrix(c(1, 4, 2, 2, 5, 3, 3, 6, 4, 4, 7, 5, 5, 1, 6, 6, 2, 7,
                          7, 3, 1, 1, 5, 6, 2, 6, 7, 3, 7, 1, 4, 1, 2, 5, 2, 3,
                          6, 3, 4, 7, 4, 5), 14, 3, byrow = T)
  } else if (all(D == 7, selection == 2)) {
    sequences <- matrix(c(1, 2, 4, 7, 2, 3, 5, 1, 3, 4, 6, 2, 4, 5, 7, 3, 5, 6,
                          1, 4, 6, 7, 2, 5, 7, 1, 3, 6, 1, 7, 5, 2, 2, 1, 6, 3,
                          3, 2, 7, 4, 4, 3, 1, 5, 5, 4, 2, 6, 6, 5, 3, 7, 7, 6,
                          4, 1), 14, 4, byrow = T)
  } else if (all(D == 7, selection == 3)) {
    sequences <- matrix(c(1, 3, 4, 2, 6, 2, 4, 5, 3, 7, 3, 5, 6, 4, 1, 4, 6, 7,
                          5, 2, 5, 7, 1, 6, 3, 6, 1, 2, 7, 4, 7, 2, 3, 1, 5),
                        7, 5, byrow = T)
  } else if (all(D == 8, selection == 1)) {
    sequences <- matrix(c(1, 5, 2, 2, 6, 3, 3, 7, 4, 4, 8, 5, 5, 1, 6, 6, 2, 7,
                          7, 3, 8, 8, 4, 1, 1, 7, 6, 2, 8, 7, 3, 1, 8, 4, 2, 1,
                          5, 3, 2, 6, 4, 3, 7, 5, 4, 8, 6, 5), 16, 3, byrow = T)
  } else if (all(D == 8, selection == 2)) {
    sequences <- matrix(c(1, 3, 2, 5, 2, 4, 3, 6, 3, 5, 4, 7, 4, 6, 5, 8, 5, 7,
                          6, 1, 6, 8, 7, 2, 7, 1, 8, 3, 8, 2, 1, 4, 1, 2, 6, 4,
                          2, 3, 7, 5, 3, 4, 8, 6, 4, 5, 1, 7, 5, 6, 2, 8, 6, 7,
                          3, 1, 7, 8, 4, 2, 8, 1, 5, 3), 16, 4, byrow = T)
  } else if (all(D == 8, selection == 3)) {
    sequences <- matrix(c(1, 2, 4, 3, 6, 2, 3, 5, 4, 7, 3, 4, 6, 5, 8, 4, 5, 7,
                          6, 1, 5, 6, 8, 7, 2, 6, 7, 1, 8, 3, 7, 8, 2, 1, 4, 8,
                          1, 3, 2, 5), 8, 5, byrow = T)
  } else if (all(D == 9, selection == 1)) {
    sequences <- matrix(c(1, 4, 9, 2, 5, 1, 3, 6, 2, 4, 7, 3, 5, 8, 4, 6, 9, 5,
                          7, 1, 6, 8, 2, 7, 9, 3, 8, 1, 7, 8, 2, 8, 9, 3, 9, 1,
                          4, 1, 2, 5, 2, 3, 6, 3, 4, 7, 4, 5, 8, 5, 6, 9, 6, 7),
                        18, 3, byrow = T)
  } else if (all(D == 9, selection == 2)) {
    sequences <- matrix(c(1, 2, 5, 3, 2, 3, 6, 4, 3, 4, 7, 5, 4, 5, 8, 6, 5, 6,
                          9, 7, 6, 7, 1, 8, 7, 8, 2, 9, 8, 9, 3, 1, 9, 1, 4, 2,
                          1, 6, 3, 7, 2, 7, 4, 8, 3, 8, 5, 9, 4, 9, 6, 1, 5, 1,
                          7, 2, 6, 2, 8, 3, 7, 3, 9, 4, 8, 4, 1, 5, 9, 5, 2, 6),
                        18, 4, byrow = T)
  } else {
    sequences <- matrix(c(1, 2, 4, 3, 6, 2, 3, 5, 4, 7, 3, 4, 6, 5, 8, 4, 5, 7,
                          6, 9, 5, 6, 8, 7, 1, 6, 7, 9, 8, 2, 7, 8, 1, 9, 3, 8,
                          9, 2, 1, 4, 9, 1, 3, 2, 5), 9, 5, byrow = T)
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
