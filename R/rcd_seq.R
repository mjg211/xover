#' Row-column design specification
#'
#' Specifies the row-column design corresponding to an input cross-over design.
#'
#' \code{rcd_seq()} supports the specification of row-column designs. Precisely,
#' a crossover design is provided (see \code{sequences}) and the corresponding
#' row-column design for the chosen model (see \code{model}) is returned.
#' The value of \code{model} must be an integer between one and nine inclusive,
#' corresponding to the following models:
#'
#' \itemize{
#' \item 1. Standard additive model;
#' \item 2. Second-order carry-over effects;
#' \item 3. Full-set of interactions;
#' \item 4. Self-adjacency model;
#' \item 5. Placebo model;
#' \item 6. No carry-over into self model;
#' \item 7. Treatment decay model;
#' \item 8. Proportionality model;
#' \item 9. No carry-over effects.
#' }
#'
#' See the package vignette for further details on these models.
#'
#' @param sequences An object of class \code{xover_seq}, describing the
#' cross-over design for which the row-column design will be computed.
#' @param model A single \code{\link[base]{numeric}} integer between one and
#' nine inclusive, specifying which model to compute the row-column design for.
#' See \strong{Details} for further information. Defaults to \code{1}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[base]{matrix}} of class \code{xover_rcd} detailing the
#' determined row-column design.
#' @examples
#' # Row-column design for a three-treatment three-period Latin square
#' latin <- seq_latin(D = 3)
#' rcd   <- rcd_seq(latin)
#' @author Based on code from the \code{\link[Crossover]{Crossover}} package by
#' Kornelius Rohmeyer.
#' @export
rcd_seq <- function(sequences, model = 1, summary = T) {

  ##### Error checking #########################################################

  check_sequences(sequences)
  check_integer_range(model, "model", c(0, 10), 1)
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the required calculations...")
  }
  if (tibble::is_tibble(sequences)) {
    sequences    <- tibble_to_matrix(sequences)
  }
  labels         <- unique(as.vector(sequences))
  D              <- length(labels)
  if (!all(sort(labels) == 1:D)) {
    sequences    <- convert_labels(sequences, D, 1:D, labels)
  }
  K              <- nrow(sequences)
  rcd            <- sequences
  if (model %in% 1:7) {
    for (k in 2:K) {
      rcd[k, ]   <- sequences[k, ] + sequences[k - 1, ]*D
    }
  } else if (model == 8) {
    rcd[2, ]     <- sequences[2, ] + sequences[1, ]*D
    if (K >= 3) {
      for (k in 3:K) {
        rcd[k, ] <- sequences[k, ] + sequences[k - 1, ]*D +
                      sequences[k - 2, ]*D^2
      }
    }
  }
  if (summary) {
    message("...completed the required calculations. Preparing outputs...")
  }
  class(rcd)     <- c("matrix", "xover_rcd")

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(rcd)

}
