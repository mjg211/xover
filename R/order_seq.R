#' Sort cross-over design sequences
#'
#' Sorts a set of cross-over design sequences in to a canonical order.
#'
#' Accepts an object of \code{\link[base]{class}} \code{xover_seq} (see
#' \code{sequences}), for which it will then sort the sequences in to a
#' canonical order (i.e., alphabetical or lexographical as appropriate).
#'
#' When comparing cross-over designs with a larger number of sequences or
#' periods this ordering eases the ability to check whether two sequences are
#' equal.
#'
#' @param sequences An object of \code{\link[base]{class}} \code{xover_seq}; a
#' set of cross-over design sequences.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return An object of \code{\link[base]{class}} \code{xover_seq}, with
#' canonically ordered sequences.
#' @author Based on code from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @seealso The \code{seq_#()} functions within \code{\link[xover]{xover}}.
#' @export
order_seq <- function(sequences, summary = T) {

  ##### Input checking #########################################################

  check_sequences(sequences)
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the required calculations...")
  }
  if (tibble::is_tibble(sequences)) {
    sequences <- tibble_to_matrix(sequences)
    as_matrix <- F
  } else {
    as_matrix <- T
  }
  sequences   <- sequences[, do.call(order, lapply(1:NROW(sequences),
                                                   function(k) sequences[k, ]))]
  sequences   <- transform_to_xover(sequences, labels, as_matrix)
  if (summary) {
    message("...completed the required calculations. Preparing outputs...")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
