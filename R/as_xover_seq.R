#' Create an object of class \code{xover_seq}
#'
#' Converts a given \code{\link[base]{matrix}} to an object of
#' \code{\link[base]{class}} \code{xover_seq}.
#'
#' \code{as_xover_seq()} supports the conversion of a \code{\link[base]{matrix}}
#' (see \code{sequences}) in to an object of \code{\link[base]{class}}
#' \code{xover_seq}, for use with other relevant functions within
#' \code{\link[xover]{xover}}. The sequences can be returned in
#' \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form (see
#' \code{as_matrix}).
#'
#' Ultimately, the \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix corresponds to the treatment a
#' subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param sequences A \code{\link[base]{matrix}} whose rows and columns will be
#' interpreted as different sequences and periods respectively. Must have at
#' least two rows and two columns.
#' @param as_matrix A \code{\link[base]{logical}} variable indicating whether
#' the sequences should be returned as a \code{\link[base]{matrix}}, or a
#' \code{\link[tibble]{tibble}}. Defaults to \code{T}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return Either a \code{\link[base]{matrix}} if \code{as_matrix = T} (with
#' rows corresponding to sequences and columns to periods), or a
#' \code{\link[tibble]{tibble}} if \code{as_matrix = F} (with rows corresponding
#' to a particular period on a particular sequence). The object will be of
#' \code{\link[base]{class}} \code{xover_seq}.
#' @examples
#' # An example cross-over design
#' sequences        <- matrix(c(0, 1, 0, 1, 0, 1), 2, 3, byrow = T)
#' # Construct a cross-over design matrix of class xover_seq
#' sequences_matrix <- as_xover_seq(sequences)
#' # Return a tibble of class xover_seq
#' sequences_tibble <- as_xover_seq(sequences, as_matrix = F)
#' @seealso The \code{seq_#()} functions within \code{\link[xover]{xover}}.
#' @export
as_xover_seq <- function(sequences, as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  if (!is.matrix(sequences)) {
    stop("sequences must be a matrix.")
  }
  if (nrow(sequences) == 1) {
    stop("sequences must have at least two rows.")
  }
  if (ncol(sequences) == 1) {
    stop("sequences must have at least two columns.")
  }
  check_logical(as_matrix, "as_matrix")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the required calculations...")
  }
  sequences <- transform_to_xover(sequences, unique(as.vector(sequences)),
                                  as_matrix)
  if (summary) {
    message("...completed the required calculations. Preparing outputs...")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
