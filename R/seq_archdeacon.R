#' Archdeacon \emph{et al.} (1980) cross-over design specification
#'
#' Specifies a cross-over design from Archdeacon \emph{et al.} (1980).
#'
#' \code{seq_archdeacon()} supports the specification of cross-over designs from
#' Archdeacon \emph{et al.} (1980). Sequences are supported for any chosen
#' treatment labels (see \code{labels}). In addition, the design can be
#' returned in \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form
#' (see \code{as_matrix}).
#'
#' Precisely, a set of nine sequences are returned, for a design with nine
#' periods and nine treatments. Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix corresponds to the treatment a
#' subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param labels A \code{\link[base]{vector}} of labels for the treatments.
#' Should be of \code{\link[base]{length}} nine, containing unique elements.
#' Defaults to \code{0:8}.
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
#' # Archdeacon et al. (1980) design for the default parameters
#' archdeacon         <- seq_archdeacon()
#' # Using different labels
#' archdeacon_LETTERS <- seq_archdeacon(labels = LETTERS[1:9])
#' # Returning in tibble form
#' archdeacon_tibble  <- seq_archdeacon(as_matrix = F)
#' @references Archdeacon DS, Dinitz JH, Stinson DR, Tillson TW (1980) Some new
#' row-complete latin squares. \emph{J Comb Theory A} \strong{29:}395-398.
#' @author Based on data from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
seq_archdeacon <- function(labels = 0:8, as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_labels(labels, 9)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  sequences <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 2, 4, 1, 6, 3, 9, 8, 7, 5, 3,
                        6, 5, 7, 4, 2, 9, 1, 8, 4, 7, 6, 9, 2, 8, 5, 3, 1, 5, 1,
                        9, 3, 8, 4, 6, 2, 7, 6, 8, 2, 5, 9, 7, 1, 4, 3, 7, 9, 4,
                        8, 6, 1, 3, 5, 2, 8, 3, 7, 2, 1, 5, 4, 9, 6, 9, 5, 8, 1,
                        7, 3, 2, 6, 4), 9, 9, byrow = T)
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  sequences <- convert_labels(sequences, 9, labels, 1:9)
  sequences <- transform_to_xover(sequences, labels, as_matrix)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
