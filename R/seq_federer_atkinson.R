#' Federer and Atkinson (1964) cross-over design specification
#'
#' Specifies cross-over designs from Federer and Atkinson (1964).
#'
#' \code{seq_federer_atkinson()} supports the specification of designs from
#' Federer and Atkinson (1964). Designs for three, four, or five treatments
#' (see \code{D}) are supported, for any chosen treatment labels (see
#' \code{labels}). In addition, the designs can be returned in
#' \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form (see
#' \code{as_matrix}).
#'
#' Precisely, for \code{D = 3}, there are two designs available (accessible by
#' setting \code{selection} equal to one or two), for \code{D = 4}, there are
#' two designs available (accessible by setting \code{selection} equal to one or
#' two), and for \code{D = 5}, there is one design available (accessible by
#' setting \code{selection} equal to \code{1}). Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' sequence matrix corresponds to the treatment a subject on the
#' \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be either three, four, or five.
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
#' # Federer and Atkinson (1964) designs for three treatments
#' federer_atkinson1        <- seq_federer_atkinson()
#' federer_atkinson2        <- seq_federer_atkinson(selection = 2)
#' # Using different labels
#' federer_atkinson1_ABC    <- seq_federer_atkinson(labels = LETTERS[1:3])
#' federer_atkinson2_ABC    <- seq_federer_atkinson(selection = 2,
#'                                                  labels = LETTERS[1:3])
#' # Returning in tibble form
#' federer_atkinson1_tibble <- seq_federer_atkinson(as_matrix = F)
#' federer_atkinson2_tibble <- seq_federer_atkinson(selection = 2,
#'                                                  as_matrix = F)
#' @references Federer WT, Atkinson GF (1964) Tied-double-change-over designs.
#' \emph{Biometrics} \strong{20:}168-81.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_federer_atkinson <- function(D = 3, selection = 1, labels = 0:(D - 1),
                                 as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(2, 6), 1)
  check_selection(selection, c(rep(NA, 2), 2, 2, 1), D)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (all(D == 3, selection == 1)) {
    sequences <- matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 3, 2, 1, 2, 1,
                          3, 2, 3, 2, 1, 3), 6, 4, byrow = T)
  } else if (all(D == 3, selection == 2)) {
    sequences <- matrix(c(1, 2, 3, 1, 3, 2, 1, 2, 3, 1, 2, 1, 3, 2, 3, 1, 2, 3,
                          2, 1, 3, 1, 3, 2, 1, 2, 3, 1, 2, 1, 3, 2, 3, 1, 2, 3,
                          2, 1, 3, 1, 2, 3), 6, 7, byrow = T)
  } else if (all(D == 4, selection == 1)) {
    sequences <- matrix(c(1, 2, 3, 4, 1, 2, 1, 4, 3, 2, 3, 4, 1, 2, 3, 4, 3, 2,
                          1, 4, 1, 4, 2, 3, 1, 2, 3, 1, 4, 2, 3, 2, 4, 1, 3, 4,
                          1, 3, 2, 4, 1, 3, 4, 2, 1, 2, 4, 3, 1, 2, 3, 1, 2, 4,
                          3, 4, 2, 1, 3, 4), 12, 5, byrow = T)
  } else if (all(D == 4, selection == 2)) {
    sequences <- matrix(c(1, 2, 3, 4, 1, 4, 3, 2, 1, 2, 1, 4, 3, 2, 3, 4, 1, 2,
                          3, 4, 1, 2, 3, 2, 1, 4, 3, 4, 3, 2, 1, 4, 1, 2, 3, 4,
                          1, 4, 2, 3, 1, 3, 2, 4, 1, 2, 3, 1, 4, 2, 4, 1, 3, 2,
                          3, 2, 4, 1, 3, 1, 4, 2, 3, 4, 1, 3, 2, 4, 2, 3, 1, 4,
                          1, 3, 4, 2, 1, 2, 4, 3, 1, 2, 4, 3, 1, 2, 1, 3, 4, 2,
                          3, 1, 2, 4, 3, 4, 2, 1, 3, 4, 2, 1, 3, 4, 3, 1, 2, 4),
                        12, 9, byrow = T)
  } else {
    sequences <- matrix(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3,
                          4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 3, 5, 2, 4, 1,
                          2, 4, 1, 3, 5, 2, 3, 5, 2, 4, 1, 3, 4, 1, 3, 5, 2, 4,
                          5, 2, 4, 1, 3, 5, 1, 4, 2, 5, 3, 1, 2, 5, 3, 1, 4, 2,
                          3, 1, 4, 2, 5, 3, 4, 2, 5, 3, 1, 4, 5, 3, 1, 4, 2, 5,
                          1, 5, 4, 3, 2, 1, 2, 1, 5, 4, 3, 2, 3, 2, 1, 5, 4, 3,
                          4, 3, 2, 1, 5, 4, 5, 4, 3, 2, 1, 5), 20, 6, byrow = T)
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
