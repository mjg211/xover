#' Iqbal and Jones (1994) cross-over design specification
#'
#' Specifies cross-over designs from Iqbal and Jones (1994).
#'
#' \code{seq_iqbal_jones()} supports the specification of designs from
#' Iqbal and Jones (1994). Designs for three through ten treatments (see
#' \code{D}) are supported, for any chosen treatment labels (see \code{labels}).
#' In addition, the designs can be returned in \code{\link[base]{matrix}} or
#' \code{\link[tibble]{tibble}} form (see \code{as_matrix}).
#'
#' Precisely, for \code{D} equal to three through ten, there are five, three,
#' four, five, six, six, one, and one designs available respectively (accessible
#' by setting \code{selection} equal to one through six as appropriate).
#' Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' cross-over design matrix corresponds to the treatment a subject on the
#' \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be an integer between three and ten
#' inclusive. Defaults to \code{3}.
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
#' # Iqbal and Jones (1994) designs for four treatments
#' iqbal_jones1        <- seq_iqbal_jones(D = 4)
#' iqbal_jones2        <- seq_iqbal_jones(D = 4, selection = 2)
#' iqbal_jones3        <- seq_iqbal_jones(D = 4, selection = 3)
#' # Using different labels
#' iqbal_jones1_ABCD   <- seq_iqbal_jones(D = 4, labels = LETTERS[1:4])
#' iqbal_jones2_ABCD   <- seq_iqbal_jones(D = 4, selection = 2,
#'                                        labels = LETTERS[1:4])
#' iqbal_jones3_ABCD   <- seq_iqbal_jones(D = 4, selection = 3,
#'                                        labels = LETTERS[1:4])
#' # Returning in tibble form
#' iqbal_jones1_tibble <- seq_iqbal_jones(D = 4, as_matrix = F)
#' iqbal_jones2_tibble <- seq_iqbal_jones(D = 4, selection = 2, as_matrix = F)
#' iqbal_jones3_tibble <- seq_iqbal_jones(D = 4, selection = 3, as_matrix = F)
#' @references Iqbal I, Jones B (1994) Efficient repeated measurements designs
#' with equal and unequal period sizes. \emph{J Stat Plan Infer}
#' \strong{42:}79-88.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_iqbal_jones <- function(D = 3, selection = 1, labels = 0:(D - 1),
                            as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(2, 11), 1)
  check_selection(selection, c(NA, NA, 5, 3, 4, 5, 6, 6, 1, 1), D)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (all(D == 3, selection == 1)) {
    sequences <- matrix(c(1, 2, 3, 2, 3, 1, 3, 1, 2, 1, 2, 3, 2, 3, 1, 3, 1, 2,
                          1, 3, 2, 2, 1, 3, 3, 2, 1), 9, 3, byrow = T)
  } else if (all(D == 3, selection == 2)) {
    sequences <- matrix(c(1, 2, 3, 2, 2, 3, 1, 3, 3, 1, 2, 1), 3, 4, byrow = T)
  } else if (all(D == 3, selection == 3)) {
    sequences <- matrix(c(1, 2, 3, 2, 1, 2, 3, 1, 3, 2, 3, 1, 2, 1, 3), 3, 5,
                        byrow = T)
  } else if (all(D == 3, selection == 4)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 2, 2, 3, 1, 2, 1, 3, 3, 1, 2, 3, 2, 1),
                        3, 6, byrow = T)
  } else if (all(D == 3, selection == 5)) {
    sequences <- matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3,
                          1, 2, 3, 1, 3, 2, 1, 3, 2, 1, 2, 1, 3, 2, 1, 3, 2, 3,
                          2, 1, 3, 2, 1, 3), 6, 7, byrow = T)
  } else if (all(D == 4, selection == 1)) {
    sequences <- matrix(c(1, 2, 4, 2, 3, 1, 3, 4, 2, 4, 1, 3, 1, 3, 2, 2, 4, 3,
                          3, 1, 4, 4, 2, 1), 8, 3, byrow = T)
  } else if (all(D == 4, selection == 2)) {
    sequences <- matrix(c(1, 2, 4, 3, 2, 4, 2, 3, 1, 4, 3, 1, 3, 4, 2, 1, 4, 2,
                          4, 1, 3, 2, 1, 3, 1, 2, 4, 3, 4, 3, 2, 3, 1, 4, 1, 4,
                          3, 4, 2, 1, 2, 1, 4, 1, 3, 2, 3, 2, 1, 2, 4, 3, 1, 2,
                          2, 3, 1, 4, 2, 3, 3, 4, 2, 1, 3, 4, 4, 1, 3, 2, 4, 1),
                        12, 6, byrow = T)
  } else if (all(D == 4, selection == 3)) {
    sequences <- matrix(c(1, 2, 4, 2, 3, 5, 3, 4, 1, 4, 5, 2, 5, 1, 3, 1, 4, 3,
                          2, 5, 4, 3, 1, 5, 4, 2, 1, 5, 3, 2), 10, 3, byrow = T)
  } else if (all(D == 5, selection == 1)) {
    sequences <- matrix(c(1, 2, 5, 4, 2, 3, 1, 5, 3, 4, 2, 1, 4, 5, 3, 2, 5, 1,
                          4, 3), 5, 4, byrow = T)
  } else if (all(D == 5, selection == 2)) {
    sequences <- matrix(c(1, 2, 5, 4, 2, 3, 1, 5, 3, 4, 2, 1, 4, 5, 3, 2, 5, 1,
                          4, 3, 1, 5, 2, 3, 2, 1, 3, 4, 3, 2, 4, 5, 4, 3, 5, 1,
                          5, 4, 1, 2), 10, 4, byrow = T)
  } else if (all(D == 5, selection == 3)) {
    sequences <- matrix(c(1, 2, 4, 3, 5, 2, 3, 5, 4, 1, 3, 4, 1, 5, 2, 4, 5, 2,
                          1, 3, 5, 1, 3, 2, 4, 1, 2, 5, 4, 3, 2, 3, 1, 5, 4, 3,
                          4, 2, 1, 5, 4, 5, 3, 2, 1, 5, 1, 4, 3, 2, 1, 4, 2, 3,
                          5, 2, 5, 3, 4, 1, 3, 1, 4, 5, 2, 4, 2, 5, 1, 3, 5, 3,
                          1, 2, 4), 15, 5, byrow = T)
  } else if (all(D == 5, selection == 4)) {
    sequences <- matrix(c(1, 2, 3, 5, 4, 2, 2, 3, 4, 1, 5, 3, 3, 4, 5, 2, 1, 4,
                          4, 5, 1, 3, 2, 5, 5, 1, 2, 4, 3, 1), 5, 6, byrow = T)
  } else if (all(D == 6, selection == 1)) {
    sequences <- matrix(c(1, 2, 5, 2, 3, 6, 3, 4, 1, 4, 5, 2, 5, 6, 3, 6, 1, 4,
                          1, 3, 2, 2, 4, 3, 3, 5, 4, 4, 6, 5, 5, 1, 6, 6, 2, 1,
                          1, 4, 2, 2, 5, 3, 3, 6, 4, 4, 1, 5, 5, 2, 6, 6, 3, 1),
                        18, 3, byrow = T)
  } else if (all(D == 6, selection == 2)) {
    sequences <- matrix(c(1, 2, 6, 5, 2, 3, 1, 6, 3, 4, 2, 1, 4, 5, 3, 2, 5, 6,
                          4, 3, 6, 1, 5, 4), 6, 4, byrow = T)
  } else if (all(D == 6, selection == 3)) {
    sequences <- matrix(c(1, 2, 4, 3, 2, 3, 5, 4, 3, 4, 6, 5, 4, 5, 1, 6, 5, 6,
                          2, 1, 6, 1, 3, 2, 1, 3, 6, 4, 2, 4, 1, 5, 3, 5, 2, 6,
                          4, 6, 3, 1, 5, 1, 4, 2, 6, 2, 5, 3, 1, 2, 5, 3, 2, 3,
                          6, 4, 3, 4, 1, 5, 4, 5, 2, 6, 5, 6, 3, 1, 6, 1, 4, 2),
                        18, 4, byrow = T)
  } else if (all(D == 6, selection == 4)) {
    sequences <- matrix(c(1, 2, 6, 5, 3, 2, 3, 1, 6, 4, 3, 4, 2, 1, 5, 4, 5, 3,
                          2, 6, 5, 6, 4, 3, 1, 6, 1, 5, 4, 2, 1, 3, 6, 5, 2, 2,
                          4, 1, 6, 3, 3, 5, 2, 1, 4, 4, 6, 3, 2, 5, 5, 1, 4, 3,
                          6, 6, 2, 5, 4, 1), 12, 5, byrow = T)
  } else if (all(D == 6, selection == 5)) {
    sequences <- matrix(c(1, 3, 2, 5, 6, 4, 5, 2, 4, 3, 6, 1, 5, 6, 3, 5, 4, 1,
                          2, 6, 1, 4, 6, 5, 2, 3, 1, 2, 5, 1, 6, 3, 4, 2, 3, 6,
                          2, 1, 4, 5, 3, 4, 1, 2, 6, 3, 5, 4, 6, 2, 3, 1, 4, 6,
                          5, 1, 3, 4, 2, 5, 1, 6, 2, 4, 5, 3, 6, 2, 1, 3, 5, 6,
                          4, 1, 3, 2, 4, 6, 1, 5, 2, 4, 3, 5, 1, 5, 6, 3, 2, 4,
                          1, 2, 6, 1, 4, 3, 5, 2, 3, 1, 2, 5, 4, 6, 3, 4, 2, 3,
                          6, 5, 1, 4, 5, 3, 4, 1, 6, 2, 5, 6, 4, 5, 2, 1, 3, 6,
                          1, 6, 2, 5, 3, 4, 2, 2, 1, 3, 6, 4, 5, 3, 3, 2, 4, 1,
                          5, 6, 4, 4, 3, 5, 2, 6, 1, 5, 5, 4, 6, 3, 1, 2, 6, 6,
                          5, 1, 4, 2, 3, 1, 1, 6, 2, 5, 3, 4, 3, 2, 1, 3, 6, 4,
                          5, 4, 3, 2, 4, 1, 5, 6, 5, 4, 3, 5, 2, 6, 1, 6, 5, 4,
                          6, 3, 1, 2, 1, 6, 5, 1, 4, 2, 3, 2), 30, 7, byrow = T)
  } else if (all(D == 7, selection == 1)) {
    sequences <- matrix(c(1, 4, 6, 7, 2, 5, 7, 1, 3, 6, 1, 2, 4, 7, 2, 3, 5, 1,
                          3, 4, 6, 2, 4, 5, 7, 3, 5, 6), 7, 4, byrow = T)
  } else if (all(D == 7, selection == 2)) {
    sequences <- matrix(c(1, 2, 3, 5, 2, 3, 4, 6, 3, 4, 5, 7, 4, 5, 6, 1, 5, 6,
                          7, 2, 6, 7, 1, 3, 7, 1, 2, 4, 1, 3, 6, 2, 2, 4, 7, 3,
                          3, 5, 1, 4, 4, 6, 2, 5, 5, 7, 3, 6, 6, 1, 4, 7, 7, 2,
                          5, 1, 1, 7, 4, 2, 2, 1, 5, 3, 3, 2, 6, 4, 4, 3, 7, 5,
                          5, 4, 1, 6, 6, 5, 2, 7, 7, 6, 3, 1), 21, 4, byrow = T)
  } else if (all(D == 7, selection == 3)) {
    sequences <- matrix(c(1, 2, 7, 4, 3, 2, 3, 1, 5, 4, 3, 4, 2, 6, 5, 4, 5, 3,
                          7, 6, 5, 6, 4, 1, 7, 6, 7, 5, 2, 1, 7, 1, 6, 3, 2, 1,
                          3, 6, 7, 5, 2, 4, 7, 1, 6, 3, 5, 1, 2, 7, 4, 6, 2, 3,
                          1, 5, 7, 3, 4, 2, 6, 1, 4, 5, 3, 7, 2, 5, 6, 4), 14,
                        5, byrow = T)
  } else if (all(D == 7, selection == 4)) {
    sequences <- matrix(c(1, 2, 5, 7, 6, 3, 2, 3, 6, 1, 7, 4, 3, 4, 7, 2, 1, 5,
                          4, 5, 1, 3, 2, 6, 5, 6, 2, 4, 3, 7, 6, 7, 3, 5, 4, 1,
                          7, 1, 4, 6, 5, 2), 7, 6, byrow = T)
  } else if (all(D == 7, selection == 5)) {
    sequences <- matrix(c(1, 2, 5, 7, 6, 3, 2, 3, 6, 1, 7, 4, 3, 4, 7, 2, 1, 5,
                          4, 5, 1, 3, 2, 6, 5, 6, 2, 4, 3, 7, 6, 7, 3, 5, 4, 1,
                          7, 1, 4, 6, 5, 2, 1, 2, 7, 4, 3, 6, 2, 3, 1, 5, 4, 7,
                          3, 4, 2, 6, 5, 1, 4, 5, 3, 7, 6, 2, 5, 6, 4, 1, 7, 3,
                          6, 7, 5, 2, 1, 4, 7, 1, 6, 3, 2, 5, 1, 4, 6, 5, 2, 7,
                          2, 5, 7, 6, 3, 1, 3, 6, 1, 7, 4, 2, 4, 7, 2, 1, 5, 3,
                          5, 1, 3, 2, 6, 4, 6, 2, 4, 3, 7, 5, 7, 3, 5, 4, 1, 6),
                        21, 6, byrow = T)
  } else if (all(D == 7, selection == 6)) {
    sequences <- matrix(c(1, 2, 4, 7, 6, 3, 1, 2, 3, 5, 1, 7, 4, 2, 3, 4, 6, 2,
                          1, 5, 3, 4, 5, 7, 3, 2, 6, 4, 5, 6, 1, 4, 3, 7, 5, 6,
                          7, 2, 5, 4, 1, 6, 7, 1, 3, 6, 5, 2, 7, 1, 6, 5, 2, 4,
                          7, 1, 2, 7, 6, 3, 5, 1, 2, 3, 1, 7, 4, 6, 2, 3, 4, 2,
                          1, 5, 7, 3, 4, 5, 3, 2, 6, 1, 4, 5, 6, 4, 3, 7, 2, 5,
                          6, 7, 5, 4, 1, 3, 6, 7, 1, 4, 6, 5, 3, 7, 1, 2, 5, 7,
                          6, 4, 1, 2, 3, 6, 1, 7, 5, 2, 3, 4, 7, 2, 1, 6, 3, 4,
                          5, 1, 3, 2, 7, 4, 5, 6, 2, 4, 3, 1, 5, 6, 7, 3, 5, 4,
                          2, 6, 7), 21, 7, byrow = T)
  } else if (all(D == 8, selection == 1)) {
    sequences <- matrix(c(1, 5, 7, 8, 2, 6, 8, 1, 3, 7, 1, 2, 4, 8, 2, 3, 5, 1,
                          3, 4, 6, 2, 4, 5, 7, 3, 5, 6, 8, 4, 6, 7), 8, 4,
                        byrow = T)
  } else if (all(D == 8, selection == 2)) {
    sequences <- matrix(c(1, 7, 2, 3, 2, 8, 3, 4, 3, 1, 4, 5, 4, 2, 5, 6, 5, 3,
                          6, 7, 6, 4, 7, 8, 7, 5, 8, 1, 8, 6, 1, 2, 1, 3, 6, 5,
                          2, 4, 7, 6, 3, 5, 8, 7, 4, 6, 1, 8, 5, 7, 2, 1, 6, 8,
                          3, 2, 7, 1, 4, 3, 8, 2, 5, 4, 1, 3, 8, 4, 2, 4, 1, 5,
                          3, 5, 2, 6, 4, 6, 3, 7, 5, 7, 4, 8, 6, 8, 5, 1, 7, 1,
                          6, 2, 8, 2, 7, 3), 24, 4, byrow = T)
  } else if (all(D == 8, selection == 3)) {
    sequences <- matrix(c(1, 2, 6, 4, 3, 2, 3, 7, 5, 4, 3, 4, 8, 6, 5, 4, 5, 1,
                          7, 6, 5, 6, 2, 8, 7, 6, 7, 3, 1, 8, 7, 8, 4, 2, 1, 8,
                          1, 5, 3, 2, 1, 4, 8, 5, 7, 2, 5, 1, 6, 8, 3, 6, 2, 7,
                          1, 4, 7, 3, 8, 2, 5, 8, 4, 1, 3, 6, 1, 5, 2, 4, 7, 2,
                          6, 3, 5, 8, 3, 7, 4, 6), 16, 5, byrow = T)
  } else if (all(D == 8, selection == 4)) {
    sequences <- matrix(c(1, 2, 4, 3, 8, 6, 2, 3, 5, 4, 1, 7, 3, 4, 6, 5, 2, 8,
                          4, 5, 7, 6, 3, 1, 5, 6, 8, 7, 4, 2, 6, 7, 1, 8, 5, 3,
                          7, 8, 2, 1, 6, 4, 8, 1, 3, 2, 7, 5), 8, 6, byrow = T)
  } else if (all(D == 8, selection == 5)) {
    sequences <- matrix(c(1, 4, 5, 3, 2, 6, 8, 2, 5, 6, 4, 3, 7, 1, 3, 6, 7, 5,
                          4, 8, 2, 4, 7, 8, 6, 5, 1, 3, 5, 8, 1, 7, 6, 2, 4, 6,
                          1, 2, 8, 7, 3, 5, 7, 2, 3, 1, 8, 4, 6, 8, 3, 4, 2, 1,
                          5, 7), 8, 7, byrow = T)
  } else if (all(D == 8, selection == 6)) {
    sequences <- matrix(c(1, 6, 5, 7, 8, 4, 2, 2, 7, 6, 8, 1, 5, 3, 3, 8, 7, 1,
                          2, 6, 4, 4, 1, 8, 2, 3, 7, 5, 5, 2, 1, 3, 4, 8, 6, 6,
                          3, 2, 4, 5, 1, 7, 7, 4, 3, 5, 6, 2, 8, 8, 5, 4, 6, 7,
                          3, 1, 1, 2, 4, 3, 7, 5, 8, 2, 3, 5, 4, 8, 6, 1, 3, 4,
                          6, 5, 1, 7, 2, 4, 5, 7, 6, 2, 8, 3, 5, 6, 8, 7, 3, 1,
                          4, 6, 7, 1, 8, 4, 2, 5, 7, 8, 2, 1, 5, 3, 6, 8, 1, 3,
                          2, 6, 4, 7, 1, 4, 5, 3, 8, 2, 6, 2, 5, 6, 4, 1, 3, 7,
                          3, 6, 7, 5, 2, 4, 8, 4, 7, 8, 6, 3, 5, 1, 5, 8, 1, 7,
                          4, 6, 2, 6, 1, 2, 8, 5, 7, 3, 7, 2, 3, 1, 6, 8, 4, 8,
                          3, 4, 2, 7, 1, 5), 24, 7, byrow = T)
  } else if (all(D == 9, selection == 1)) {
    sequences <- matrix(c(1, 2, 4, 2, 3, 5, 3, 4, 6, 4, 5, 7, 5, 6, 8, 6, 7, 9,
                          7, 8, 1, 8, 9, 2, 9, 1, 3, 1, 6, 5, 2, 7, 6, 3, 8, 7,
                          4, 9, 8, 5, 1, 9, 6, 2, 1, 7, 3, 2, 8, 4, 3, 9, 5, 4,
                          1, 8, 2, 2, 9, 3, 3, 1, 4, 4, 2, 5, 5, 3, 6, 6, 4, 7,
                          7, 5, 8, 8, 6, 9, 9, 7, 1), 27, 3, byrow = T)
  } else {
    sequences <- matrix(c(1, 2, 10, 6, 8, 3, 2, 3, 1, 7, 9, 4, 3, 4, 2, 8, 10,
                          5, 4, 5, 3, 9, 1, 6, 5, 6, 4, 10, 2, 7, 6, 7, 5, 1, 3,
                          8, 7, 8, 6, 2, 4, 9, 8, 9, 7, 3, 5, 10, 9, 10, 8, 4,
                          6, 1, 10, 1, 9, 5, 7, 2), 10, 6, byrow = T)
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
