#' Russell (1991) cross-over design specification
#'
#' Specifies cross-over designs from Russell (1991).
#'
#' \code{seq_russell()} supports the specification of designs from Russell
#' (1991). Designs for five or seven treatments (see \code{D}) are supported,
#' for any chosen treatment labels (see \code{labels}). In addition, the designs
#' can be returned in \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}}
#' form (see \code{as_matrix}).
#'
#' Precisely, the \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix corresponds to the treatment a
#' subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be either five or seven. Defaults to
#' \code{5}.
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
#' # Russell (1991) design for five treatments
#' russell        <- seq_russell()
#' # Using different labels
#' russell_ABCDE  <- seq_russell(labels = LETTERS[1:5])
#' # Returning in tibble form
#' russell_tibble <- seq_russell(as_matrix = F)
#' @references Russell KR (1991) The construction of good change-over designs
#' when there are fewer units than treatments. \emph{Biometrika}
#' \strong{78:}305-313
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_russell <- function(D = 5, labels = 0:(D - 1), as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_belong(D, "D", c(5, 7), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (D == 5) {
    sequences <- matrix(c(1, 2, 3, 4, 5, 2, 4, 5, 3, 1, 3, 5, 2, 1, 4, 4, 3, 1,
                          5, 2, 5, 1, 4, 2, 3), 5, 5, byrow = T)
  } else {
    sequences <- matrix(c(1, 2, 3, 4, 5, 6, 7, 2, 6, 7, 1, 4, 3, 5, 3, 7, 4, 6,
                          2, 5, 1, 4, 1, 6, 5, 7, 2, 3, 5, 4, 2, 7, 3, 1, 6, 6,
                          3, 5, 2, 1, 7, 4, 7, 5, 1, 3, 6, 4, 2), 7, 7,
                        byrow = T)
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
