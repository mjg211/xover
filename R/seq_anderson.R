#' Anderson (2002) cross-over design specification
#'
#' Specifies a cross-over design from Anderson (2002).
#'
#' \code{seq_anderson()} supports the specification of a cross-over design from
#' Anderson (2002). Sequences are supported for any chosen treatment labels (see
#' \code{labels}). In addition, the design can be returned in
#' \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form (see
#' \code{as_matrix}).
#'
#' Precisely, a set of 42 sequences are returned, for a design with 14
#' periods and seven treatments. Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix corresponds to the treatment a
#' subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param labels A \code{\link[base]{vector}} of labels for the treatments.
#' Should be of \code{\link[base]{length}} seven, containing unique elements.
#' Defaults to \code{0:6}.
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
#' # Anderson (2002) design for the default parameters
#' anderson         <- seq_anderson()
#' # Using different labels
#' anderson_ABCDEFG <- seq_anderson(labels = LETTERS[1:7])
#' @references Anderson I (2002) Training schedule design. \emph{Personal
#' Communication}.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_anderson <- function(labels = 0:6, as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_labels(labels, 7)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  sequences <- matrix(c(1, 2, 5, 3, 7, 6, 4, 3, 2, 6, 1, 4, 5, 7, 2, 3, 6, 4, 1,
                        7, 5, 4, 3, 7, 2, 5, 6, 1, 3, 4, 7, 5, 2, 1, 6, 5, 4, 1,
                        3, 6, 7, 2, 4, 5, 1, 6, 3, 2, 7, 6, 5, 2, 4, 7, 1, 3, 5,
                        6, 2, 7, 4, 3, 1, 7, 6, 3, 5, 1, 2, 4, 6, 7, 3, 1, 5, 4,
                        2, 1, 7, 4, 6, 2, 3, 5, 7, 1, 4, 2, 6, 5, 3, 2, 1, 5, 7,
                        3, 4, 6, 1, 7, 4, 6, 2, 3, 5, 6, 7, 3, 1, 5, 4, 2, 2, 1,
                        5, 7, 3, 4, 6, 7, 1, 4, 2, 6, 5, 3, 3, 2, 6, 1, 4, 5, 7,
                        1, 2, 5, 3, 7, 6, 4, 4, 3, 7, 2, 5, 6, 1, 2, 3, 6, 4, 1,
                        7, 5, 5, 4, 1, 3, 6, 7, 2, 3, 4, 7, 5, 2, 1, 6, 6, 5, 2,
                        4, 7, 1, 3, 4, 5, 1, 6, 3, 2, 7, 7, 6, 3, 5, 1, 2, 4, 5,
                        6, 2, 7, 4, 3, 1, 1, 4, 6, 7, 5, 2, 3, 7, 4, 2, 1, 3, 6,
                        5, 2, 5, 7, 1, 6, 3, 4, 1, 5, 3, 2, 4, 7, 6, 3, 6, 1, 2,
                        7, 4, 5, 2, 6, 4, 3, 5, 1, 7, 4, 7, 2, 3, 1, 5, 6, 3, 7,
                        5, 4, 6, 2, 1, 5, 1, 3, 4, 2, 6, 7, 4, 1, 6, 5, 7, 3, 2,
                        6, 2, 4, 5, 3, 7, 1, 5, 2, 7, 6, 1, 4, 3, 7, 3, 5, 6, 4,
                        1, 2, 6, 3, 1, 7, 2, 5, 4, 1, 5, 3, 2, 4, 7, 6, 2, 5, 7,
                        1, 6, 3, 4, 2, 6, 4, 3, 5, 1, 7, 3, 6, 1, 2, 7, 4, 5, 3,
                        7, 5, 4, 6, 2, 1, 4, 7, 2, 3, 1, 5, 6, 4, 1, 6, 5, 7, 3,
                        2, 5, 1, 3, 4, 2, 6, 7, 5, 2, 7, 6, 1, 4, 3, 6, 2, 4, 5,
                        3, 7, 1, 6, 3, 1, 7, 2, 5, 4, 7, 3, 5, 6, 4, 1, 2, 7, 4,
                        2, 1, 3, 6, 5, 1, 4, 6, 7, 5, 2, 3, 1, 3, 2, 5, 6, 4, 7,
                        5, 3, 4, 1, 7, 2, 6, 2, 4, 3, 6, 7, 5, 1, 6, 4, 5, 2, 1,
                        3, 7, 3, 5, 4, 7, 1, 6, 2, 7, 5, 6, 3, 2, 4, 1, 4, 6, 5,
                        1, 2, 7, 3, 1, 6, 7, 4, 3, 5, 2, 5, 7, 6, 2, 3, 1, 4, 2,
                        7, 1, 5, 4, 6, 3, 6, 1, 7, 3, 4, 2, 5, 3, 1, 2, 6, 5, 7,
                        4, 7, 2, 1, 4, 5, 3, 6, 4, 2, 3, 7, 6, 1, 5, 1, 6, 7, 4,
                        3, 5, 2, 4, 6, 5, 1, 2, 7, 3, 2, 7, 1, 5, 4, 6, 3, 5, 7,
                        6, 2, 3, 1, 4, 3, 1, 2, 6, 5, 7, 4, 6, 1, 7, 3, 4, 2, 5,
                        4, 2, 3, 7, 6, 1, 5, 7, 2, 1, 4, 5, 3, 6, 5, 3, 4, 1, 7,
                        2, 6, 1, 3, 2, 5, 6, 4, 7, 6, 4, 5, 2, 1, 3, 7, 2, 4, 3,
                        6, 7, 5, 1, 7, 5, 6, 3, 2, 4, 1, 3, 5, 4, 7, 1, 6, 2),
                      42, 14, byrow = T)
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  sequences <- convert_labels(sequences, 7, labels, 1:7)
  sequences <- transform_to_xover(sequences, labels, as_matrix)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
