#' Pigeon (1984) cross-over design specification
#'
#' Specifies cross-over designs from Pigeon (1984).
#'
#' \code{seq_pigeon()} supports the specification of designs from
#' Pigeon (1984). Designs for three through seven treatments (see
#' \code{D}) are supported, for any chosen treatment labels (see \code{labels}).
#' In addition, the designs can be returned in \code{\link[base]{matrix}} or
#' \code{\link[tibble]{tibble}} form (see \code{as_matrix}).
#'
#' Precisely, for \code{D} equal to three through seven, there are one, four,
#' six, seven, and two designs available respectively (accessible by setting
#' \code{selection} equal to one through seven as appropriate). Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' cross-over design matrix corresponds to the treatment a subject on the
#' ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} integer between three and seven inclusive.
#' Defaults to \code{3}.
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
#' # Pigeon (1984) designs for four treatments
#' pigeon1        <- seq_pigeon(D = 4)
#' pigeon2        <- seq_pigeon(D = 4, selection = 2)
#' pigeon3        <- seq_pigeon(D = 4, selection = 3)
#' pigeon4        <- seq_pigeon(D = 4, selection = 4)
#' # Using different labels
#' pigeon1_ABCD   <- seq_pigeon(D = 4, labels = LETTERS[1:4])
#' pigeon2_ABCD   <- seq_pigeon(D = 4, selection = 2, labels = LETTERS[1:4])
#' pigeon3_ABCD   <- seq_pigeon(D = 4, selection = 3, labels = LETTERS[1:4])
#' pigeon4_ABCD   <- seq_pigeon(D = 4, selection = 4, labels = LETTERS[1:4])
#' # Returning in tibble form
#' pigeon1_tibble <- seq_pigeon(D = 4, as_matrix = F)
#' pigeon2_tibble <- seq_pigeon(D = 4, selection = 2, as_matrix = F)
#' pigeon3_tibble <- seq_pigeon(D = 4, selection = 3, as_matrix = F)
#' pigeon4_tibble <- seq_pigeon(D = 4, selection = 4, as_matrix = F)
#' @references Pigeon JG (1985) Residual effects designs for comparing
#' treatments with a control. \emph{PhD thesis, Temple University}.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_pigeon <- function(D = 3, selection = 1, labels = 0:(D - 1),
                       as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(2, 8), 1)
  check_selection(selection, c(rep(NA, 2), 1, 4, 6, 7, 2), D)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (all(D == 3, selection == 1)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 2, 2, 1, 4, 3, 1, 2, 4, 1, 3,
                          2, 4, 1, 3, 2, 1, 4, 3, 1), 9, 3, byrow = T)
  } else if (all(D == 4, selection == 1)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 2, 1, 2, 3, 1, 3, 4, 1, 4, 2,
                          2, 1, 4, 3, 1, 2, 4, 1, 3, 2, 1, 4, 3, 1, 2, 4, 1, 3,
                          2, 4, 1, 3, 2, 1, 4, 3, 1, 2, 4, 1, 3, 2, 1, 4, 3, 1),
                        18, 3, byrow = T)
  } else if (all(D == 4, selection == 2)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 2, 1, 2, 3, 1, 3, 4, 1, 4, 2,
                          2, 1, 4, 3, 1, 2, 4, 1, 3, 2, 1, 4, 3, 1, 2, 4, 1, 3,
                          2, 3, 1, 3, 4, 1, 4, 2, 1, 2, 4, 1, 3, 2, 1, 4, 3, 1,
                          2, 4, 3, 3, 2, 4, 4, 3, 2), 21, 3, byrow = T)
  } else if (all(D == 4, selection == 3)) {
    sequences <- matrix(c(1, 2, 2, 1, 3, 3, 1, 4, 4, 1, 2, 2, 1, 3, 3, 1, 4, 4,
                          1, 2, 2, 1, 3, 3, 1, 4, 4, 2, 1, 4, 3, 1, 2, 4, 1, 3,
                          2, 1, 4, 3, 1, 2, 4, 1, 3, 2, 1, 4, 3, 1, 2, 4, 1, 3,
                          2, 4, 1, 3, 2, 1, 4, 3, 1, 2, 4, 1, 3, 2, 1, 4, 3, 1,
                          2, 4, 1, 3, 2, 1, 4, 3, 1), 27, 3, byrow = T)
  } else if (all(D == 4, selection == 4)) {
    sequences <- matrix(c(1, 2, 2, 1, 3, 3, 1, 4, 4, 1, 2, 2, 1, 3, 3, 1, 4, 4,
                          1, 2, 2, 1, 3, 3, 1, 4, 4, 2, 1, 4, 3, 1, 2, 4, 1, 3,
                          2, 1, 4, 3, 1, 2, 4, 1, 3, 2, 1, 4, 3, 1, 2, 4, 1, 3,
                          2, 3, 1, 3, 4, 1, 4, 2, 1, 2, 4, 1, 3, 2, 1, 4, 3, 1,
                          2, 4, 1, 3, 2, 1, 4, 3, 1, 2, 4, 3, 3, 2, 4, 4, 3, 2),
                        30, 3, byrow = T)
  } else if (all(D == 5, selection == 1)) {
    sequences <- matrix(c(1, 3, 5, 4, 1, 4, 2, 5, 1, 5, 3, 2, 1, 2, 4, 3, 2, 1,
                          5, 4, 3, 1, 2, 5, 4, 1, 3, 2, 5, 1, 4, 3, 2, 3, 1, 4,
                          3, 4, 1, 5, 4, 5, 1, 2, 5, 2, 1, 3, 2, 3, 5, 1, 3, 4,
                          2, 1, 4, 5, 3, 1, 5, 2, 4, 1), 16, 4, byrow = T)
  } else if (all(D == 5, selection == 2)) {
    sequences <- matrix(c(1, 3, 5, 4, 1, 4, 2, 5, 1, 5, 3, 2, 1, 2, 4, 3, 1, 3,
                          5, 4, 1, 4, 2, 5, 1, 5, 3, 2, 1, 2, 4, 3, 2, 1, 5, 4,
                          3, 1, 2, 5, 4, 1, 3, 2, 5, 1, 4, 3, 2, 1, 5, 4, 3, 1,
                          2, 5, 4, 1, 3, 2, 5, 1, 4, 3, 2, 3, 1, 4, 3, 4, 1, 5,
                          4, 5, 1, 2, 5, 2, 1, 3, 2, 3, 1, 4, 3, 4, 1, 5, 4, 5,
                          1, 2, 5, 2, 1, 3, 2, 3, 5, 1, 3, 4, 2, 1, 4, 5, 3, 1,
                          5, 2, 4, 1, 2, 3, 5, 1, 3, 4, 2, 1, 4, 5, 3, 1, 5, 2,
                          4, 1), 32, 4, byrow = T)
  } else if (all(D == 5, selection == 3)) {
    sequences <- matrix(c(1, 3, 5, 4, 1, 4, 2, 5, 1, 5, 3, 2, 1, 2, 4, 3, 1, 3,
                          5, 4, 1, 4, 2, 5, 1, 5, 3, 2, 1, 2, 4, 3, 2, 1, 5, 4,
                          3, 1, 2, 5, 4, 1, 3, 2, 5, 1, 4, 3, 2, 1, 5, 4, 3, 1,
                          2, 5, 4, 1, 3, 2, 5, 1, 4, 3, 2, 3, 1, 4, 3, 4, 1, 5,
                          4, 5, 1, 2, 5, 2, 1, 3, 2, 3, 1, 4, 3, 4, 1, 5, 4, 5,
                          1, 2, 5, 2, 1, 3, 2, 3, 5, 1, 3, 4, 2, 1, 4, 5, 3, 1,
                          5, 2, 4, 1, 2, 3, 5, 1, 3, 4, 2, 1, 4, 5, 3, 1, 5, 2,
                          4, 1, 2, 3, 5, 4, 3, 4, 2, 5, 4, 5, 3, 2, 5, 2, 4, 3),
                        36, 4, byrow = T)
  } else if (all(D == 5, selection == 4)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 2, 1, 2, 4, 1, 3, 5,
                          1, 4, 2, 1, 5, 3, 2, 1, 3, 3, 1, 4, 4, 1, 5, 5, 1, 2,
                          2, 1, 5, 3, 1, 2, 4, 1, 3, 5, 1, 4, 2, 3, 1, 3, 4, 1,
                          4, 5, 1, 5, 2, 1, 2, 4, 1, 3, 5, 1, 4, 2, 1, 5, 3, 1,
                          2, 5, 4, 3, 2, 5, 4, 3, 2, 5, 4, 3), 28, 3, byrow = T)
  } else if (all(D == 5, selection == 5)) {
    sequences <- matrix(c(1, 2, 4, 1, 2, 5, 1, 3, 4, 1, 3, 2, 1, 4, 5, 1, 4, 3,
                          1, 4, 5, 1, 5, 2, 1, 5, 3, 1, 5, 4, 2, 1, 3, 2, 1, 3,
                          2, 1, 4, 3, 1, 2, 3, 1, 5, 3, 1, 4, 4, 1, 2, 4, 1, 5,
                          5, 1, 2, 5, 1, 3, 4, 2, 1, 5, 2, 1, 2, 3, 1, 4, 3, 1,
                          2, 4, 1, 3, 4, 1, 5, 4, 1, 2, 5, 1, 3, 5, 1, 3, 5, 1,
                          4, 2, 3, 5, 2, 4, 3, 2, 5, 4, 3, 2, 5, 3, 4, 2, 3, 5,
                          3, 4, 2, 4, 5, 3), 38, 3, byrow = T)
  } else if (all(D == 5, selection == 6)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 2, 1, 2, 4, 1, 3, 5,
                          1, 4, 2, 1, 5, 3, 1, 2, 5, 1, 3, 2, 1, 4, 3, 1, 5, 4,
                          2, 1, 3, 3, 1, 4, 4, 1, 5, 5, 1, 2, 2, 1, 4, 3, 1, 5,
                          4, 1, 2, 5, 1, 3, 2, 1, 5, 3, 1, 2, 4, 1, 3, 5, 1, 4,
                          2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 2, 1, 2, 4, 1, 3, 5, 1,
                          4, 2, 1, 5, 3, 1, 2, 5, 1, 3, 2, 1, 4, 3, 1, 5, 4, 1),
                        36, 3, byrow = T)
  } else if (all(D == 6, selection == 1)) {
    sequences <- matrix(c(1, 3, 6, 4, 5, 1, 4, 2, 5, 6, 1, 5, 3, 6, 2, 1, 6, 4,
                          2, 3, 1, 2, 5, 3, 4, 2, 1, 6, 4, 5, 3, 1, 2, 5, 6, 4,
                          1, 3, 6, 2, 5, 1, 4, 2, 3, 6, 1, 5, 3, 4, 3, 2, 1, 4,
                          5, 4, 3, 1, 5, 6, 5, 4, 1, 6, 2, 6, 5, 1, 2, 3, 2, 6,
                          1, 3, 4, 6, 3, 2, 1, 5, 2, 4, 3, 1, 6, 3, 5, 4, 1, 2,
                          4, 6, 5, 1, 3, 5, 2, 6, 1, 4, 4, 6, 3, 2, 1, 5, 2, 4,
                          3, 1, 6, 3, 5, 4, 1, 2, 4, 6, 5, 1, 3, 5, 2, 6, 1),
                        25, 5, byrow = T)
  } else if (all(D == 6, selection == 2)) {
    sequences <- matrix(c(1, 3, 6, 4, 5, 1, 4, 2, 5, 6, 1, 5, 3, 6, 2, 1, 6, 4,
                          2, 3, 1, 2, 5, 3, 4, 1, 3, 6, 4, 5, 1, 4, 2, 5, 6, 1,
                          5, 3, 6, 2, 1, 6, 4, 2, 3, 1, 2, 5, 3, 4, 2, 1, 6, 4,
                          5, 3, 1, 2, 5, 6, 4, 1, 3, 6, 2, 5, 1, 4, 2, 3, 6, 1,
                          5, 3, 4, 2, 1, 6, 4, 5, 3, 1, 2, 5, 6, 4, 1, 3, 6, 2,
                          5, 1, 4, 2, 3, 6, 1, 5, 3, 4, 3, 2, 1, 4, 5, 4, 3, 1,
                          5, 6, 5, 4, 1, 6, 2, 6, 5, 1, 2, 3, 2, 6, 1, 3, 4, 3,
                          2, 1, 4, 5, 4, 3, 1, 5, 6, 5, 4, 1, 6, 2, 6, 5, 1, 2,
                          3, 2, 6, 1, 3, 4, 6, 3, 2, 1, 5, 2, 4, 3, 1, 6, 3, 5,
                          4, 1, 2, 4, 6, 5, 1, 3, 5, 2, 6, 1, 4, 6, 3, 2, 1, 5,
                          2, 4, 3, 1, 6, 3, 5, 4, 1, 2, 4, 6, 5, 1, 3, 5, 2, 6,
                          1, 4, 4, 6, 3, 2, 1, 5, 2, 4, 3, 1, 6, 3, 5, 4, 1, 2,
                          4, 6, 5, 1, 3, 5, 2, 6, 1, 4, 6, 3, 2, 1, 5, 2, 4, 3,
                          1, 6, 3, 5, 4, 1, 2, 4, 6, 5, 1, 3, 5, 2, 6, 1), 50,
                        5, byrow = T)
  } else if (all(D == 6, selection == 3)) {
    sequences <- matrix(c(1, 2, 3, 6, 1, 3, 4, 2, 1, 4, 5, 3, 1, 5, 6, 4, 1, 6,
                          2, 5, 1, 2, 4, 5, 1, 3, 5, 6, 1, 4, 6, 2, 1, 5, 2, 3,
                          1, 6, 3, 4, 2, 1, 5, 4, 3, 1, 6, 5, 4, 1, 2, 6, 5, 1,
                          3, 2, 6, 1, 4, 3, 2, 1, 6, 3, 3, 1, 2, 4, 4, 1, 3, 5,
                          5, 1, 4, 6, 6, 1, 5, 2, 3, 6, 1, 2, 4, 2, 1, 3, 5, 3,
                          1, 4, 6, 4, 1, 5, 2, 5, 1, 6, 4, 5, 1, 2, 5, 6, 1, 3,
                          6, 2, 1, 4, 2, 3, 1, 5, 3, 4, 1, 6, 5, 4, 2, 1, 6, 5,
                          3, 1, 2, 6, 4, 1, 3, 2, 5, 1, 4, 3, 6, 1, 6, 3, 2, 1,
                          2, 4, 3, 1, 3, 5, 4, 1, 4, 6, 5, 1, 5, 2, 6, 1), 40,
                        4, byrow = T)
  } else if (all(D == 6, selection == 4)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2, 2, 1, 6,
                          3, 1, 2, 4, 1, 3, 5, 1, 4, 6, 1, 5, 2, 4, 1, 3, 5, 1,
                          4, 6, 1, 5, 2, 1, 6, 3, 1, 6, 5, 3, 2, 6, 4, 3, 2, 5,
                          4, 3, 6, 5, 4, 2), 20, 3, byrow = T)
  } else if (all(D == 6, selection == 5)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2, 1, 2, 4,
                          1, 3, 5, 1, 4, 6, 1, 5, 2, 1, 6, 3, 2, 1, 5, 3, 1, 6,
                          4, 1, 2, 5, 1, 3, 6, 1, 4, 2, 1, 6, 3, 1, 2, 4, 1, 3,
                          5, 1, 4, 6, 1, 5, 2, 5, 1, 3, 6, 1, 4, 2, 1, 5, 3, 1,
                          6, 4, 1, 2, 6, 1, 3, 2, 1, 4, 3, 1, 5, 4, 1, 6, 5, 1),
                        30, 3, byrow = T)
  } else if (all(D == 6, selection == 6)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2, 1, 2, 3,
                          1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2, 2, 1, 6, 3, 1, 2,
                          4, 1, 3, 5, 1, 4, 6, 1, 5, 2, 1, 6, 3, 1, 2, 4, 1, 3,
                          5, 1, 4, 6, 1, 5, 2, 4, 1, 3, 5, 1, 4, 6, 1, 5, 2, 1,
                          6, 3, 1, 2, 4, 1, 3, 5, 1, 4, 6, 1, 5, 2, 1, 6, 3, 1,
                          6, 5, 3, 2, 6, 4, 3, 2, 5, 4, 3, 6, 5, 4, 2, 6, 5, 3,
                          2, 6, 4, 3, 2, 5, 4, 3, 6, 5, 4, 2), 40, 3, byrow = T)
  } else if (all(D == 6, selection == 7)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2, 1, 2, 3,
                          1, 3, 4, 1, 4, 5, 1, 5, 6, 1, 6, 2, 1, 2, 4, 1, 3, 5,
                          1, 4, 6, 1, 5, 2, 1, 6, 3, 2, 1, 6, 3, 1, 2, 4, 1, 3,
                          5, 1, 4, 6, 1, 5, 2, 1, 5, 3, 1, 6, 4, 1, 2, 5, 1, 3,
                          6, 1, 4, 2, 1, 6, 3, 1, 2, 4, 1, 3, 5, 1, 4, 6, 1, 5,
                          2, 4, 1, 3, 5, 1, 4, 6, 1, 5, 2, 1, 6, 3, 1, 2, 5, 1,
                          3, 6, 1, 4, 2, 1, 5, 3, 1, 6, 4, 1, 2, 6, 1, 3, 2, 1,
                          4, 3, 1, 5, 4, 1, 6, 5, 1, 6, 5, 3, 2, 6, 4, 3, 2, 5,
                          4, 3, 6, 5, 4, 2), 50, 3, byrow = T)
  } else if (all(D == 7, selection == 1)) {
    sequences <- matrix(c(1, 3, 7, 4, 6, 5, 1, 4, 2, 5, 7, 6, 1, 5, 3, 6, 2, 7,
                          1, 6, 4, 7, 3, 2, 1, 7, 5, 2, 4, 3, 1, 2, 6, 3, 5, 4,
                          2, 1, 7, 4, 6, 5, 3, 1, 2, 5, 7, 6, 4, 1, 3, 6, 2, 7,
                          5, 1, 4, 7, 3, 2, 6, 1, 5, 2, 4, 3, 7, 1, 6, 3, 5, 4,
                          2, 3, 1, 4, 6, 5, 3, 4, 1, 5, 7, 6, 4, 5, 1, 6, 2, 7,
                          5, 6, 1, 7, 3, 2, 6, 7, 1, 2, 4, 3, 7, 2, 1, 3, 5, 4,
                          2, 3, 7, 1, 6, 5, 3, 4, 2, 1, 7, 6, 4, 5, 3, 1, 2, 7,
                          5, 6, 4, 1, 3, 2, 6, 7, 5, 1, 4, 3, 7, 2, 6, 1, 5, 4,
                          2, 3, 7, 4, 1, 5, 3, 4, 2, 5, 1, 6, 4, 5, 3, 6, 1, 7,
                          5, 6, 4, 7, 1, 2, 6, 7, 5, 2, 1, 3, 7, 2, 6, 3, 1, 4,
                          2, 3, 7, 4, 6, 1, 3, 4, 2, 5, 7, 1, 4, 5, 3, 6, 2, 1,
                          5, 6, 4, 7, 3, 1, 6, 7, 5, 2, 4, 1, 7, 2, 6, 3, 5, 1),
                        36, 6, byrow = T)
  } else if (all(D == 7, selection == 2)) {
    sequences <- matrix(c(2, 3, 1, 6, 3, 6, 2, 1, 6, 1, 3, 2, 1, 2, 6, 3, 2, 3,
                          1, 7, 3, 7, 2, 1, 7, 1, 3, 2, 1, 2, 7, 3, 2, 4, 1, 5,
                          4, 5, 2, 1, 5, 1, 4, 2, 1, 2, 5, 4, 2, 4, 1, 7, 4, 7,
                          2, 1, 7, 1, 4, 2, 1, 2, 7, 4, 2, 5, 1, 6, 5, 6, 2, 1,
                          6, 1, 5, 2, 1, 2, 6, 5, 3, 4, 1, 5, 4, 5, 3, 1, 5, 1,
                          4, 3, 1, 3, 5, 4, 3, 4, 1, 6, 4, 6, 3, 1, 6, 1, 4, 3,
                          1, 3, 6, 4, 3, 5, 1, 7, 5, 7, 3, 1, 7, 1, 5, 3, 1, 3,
                          7, 5, 4, 6, 1, 7, 6, 7, 4, 1, 7, 1, 6, 4, 1, 4, 7, 6,
                          5, 6, 1, 7, 6, 7, 5, 1, 7, 1, 6, 5, 1, 5, 7, 6, 2, 3,
                          5, 1, 3, 4, 6, 2, 4, 5, 7, 3, 5, 6, 1, 4, 6, 7, 2, 5,
                          7, 1, 3, 6, 1, 2, 4, 7, 2, 1, 6, 3, 3, 2, 7, 4, 4, 3,
                          1, 5, 5, 4, 2, 6, 6, 5, 3, 7, 7, 6, 4, 1, 1, 7, 5, 2),
                        54, 4, byrow = T)
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
