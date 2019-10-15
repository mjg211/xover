#' Cross-over data contingency table
#'
#' Builds a contingency table for a cross-over dataset.
#'
#' \code{contingency_table_data()} supports the calculation of a contingency
#' table for a cross-over dataset. Precisely, a dataset of class
#' \code{xover_data} is provided (see \code{data}), along with a binary outcome
#' variable within this dataset (see \code{outcome}), and the data is reduced to
#' its complete case form for this outcome. A contingency table is then
#' constructed for this complete case data.
#'
#' Note that datasets can be converted to class \code{xover_data} using
#' \code{\link[xover]{as_xover_data}}.
#'
#' @param data A \code{\link[tibble]{tibble}} of class \code{xover_data}.
#' @param outcome The chosen binary outcome variable from \code{data} upon which
#' the contingency_table will be based. Should be stated absolutely, i.e., not
#' as a \code{\link[base]{character}} string.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[tibble]{tibble}} containing the determined contingency
#' table.
#' @references Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @seealso \code{\link[xover]{as_xover_data}} for converting datasets to class
#' \code{xover_data}.
#' @export
contingency_table_data <- function(data, outcome, summary = T) {

  ##### Input checking #########################################################

  if (missing(outcome)) {
    stop("outcome must be supplied.")
  } else if (class(try(class(outcome), silent = T)) == "try-error") {
    outcome <- substitute(outcome)
  }
  check_data(data, outcome, output = F)
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the required calculations...")
  }
  contingency_table  <- NULL
  internal_data      <- complete_case_data(data, outcome, F)
  if (nrow(internal_data) > 0) {
    sequences        <- levels(internal_data$sequence)
    K                <- length(sequences)
    seq_lengths      <- numeric(K)
    for (k in 1:K) {
      seq_lengths[k] <- length(strsplit(sequences[k], split = "")[[1]])
    }
    if (length(unique(seq_lengths)) > 1) {
      if (summary) {
        warning("The complete-case data consists of sequences of different",
                " lengths. Contingency table build will be skipped.")
      }
    } else {
      J                  <- seq_lengths[1]
      responses          <- iterpc::getall(iterpc::iterpc(n = 2, r = J,
                                                          labels = c(0, 1),
                                                          ordered = T,
                                                          replace = T))
      col_responses      <- numeric(2^J)
      for (i in 1:(2^J)) {
        col_responses[i] <- paste("(", paste(responses[i, ], collapse = ", "),
                                  ")", sep = "")
      }
      contingency_table           <- tibble::as_tibble(matrix(0, K + 1,
                                                              2^J + 3))
      colnames(contingency_table) <- c("sequence", "sequence index",
                                       col_responses, "total")
      contingency_table[, 1]      <- c(sequences, "total")
      contingency_table[, 2]      <- c(levels(internal_data$`sequence index`),
                                       "")
      subjects                    <-
        unique(as.numeric(as.character(internal_data$subject)))
      for (s in subjects) {
        data_s       <- dplyr::filter(internal_data, subject == s)
        row          <- which(sequences == as.character(data_s$sequence[1]))
        responses_s  <- paste("(", paste(eval(substitute(outcome),
                                              data_s[order(data_s$period), ]),
                                         collapse = ", "), ")", sep = "")
        contingency_table[row, 2 + which(responses_s == col_responses)] <-
          contingency_table[row, 2 + which(responses_s == col_responses)] + 1
      }
      contingency_table[K + 1, 3:(2^J + 2)] <-
        colSums(contingency_table[, 3:(2^J + 2)])
      contingency_table$total     <- rowSums(contingency_table[, -(1:2)])
    }
  }
  if (summary) {
    message("...completed the required calculations. Preparing outputs...")
  }

  ##### Ouputting ##############################################################

  if (summary) {
    message("...outputting.")
  }
  return(contingency_table)

}
