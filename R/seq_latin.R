#' Latin square cross-over design specification
#'
#' Specifies Latin square cross-over designs.
#'
#' \code{seq_latin()} supports the specification of Latin square designs.
#' Designs for any number of treatments (see \code{D}) are supported, for any
#' chosen treatment labels (see \code{labels}). In addition, the designs can be
#' returned in \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form
#' (see \code{as_matrix}).
#'
#' Precisely, the \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix corresponds to the treatment a
#' subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to three. Defaults
#' to \code{2}.
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
#' # A Latin square for the default parameters
#' latin        <- seq_latin()
#' # Using different labels
#' latin_AB     <- seq_latin(labels = LETTERS[1:2])
#' # Returning in tibble form
#' latin_tibble <- seq_latin(as_matrix = F)
#' @references Bailey RA (2008) \emph{Design of Comparative Experiments}.
#' Cambridge University Press: Cambridge.
#' @references Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @export
seq_latin <- function(D = 2, labels = 0:(D - 1), as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(1, Inf), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  sequences        <- matrix(0, D, D)
  sequences[1, ]   <- labels
  for (k in 2:D) {
    sequences[k, ] <- c(sequences[k - 1, 2:D], sequences[k - 1, 1])
  }
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  sequences        <- transform_to_xover(sequences, labels, as_matrix)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
