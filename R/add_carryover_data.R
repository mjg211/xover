#' Add carryover information
#'
#' Adds carry-over information to a cross-over dataset.
#'
#' \code{add_carryover_data()} supports the addition of a column to a cross-over
#' dataset describing the first order carryover information. Precisely, a
#' dataset of class \code{xover_data} is provided (see \code{data}) and the new
#' column is constructed by determining for each outcome variable the treatment
#' received by that subject in the previous period. Those outcome variables from
#' period one are assigned the first level of \code{data$treatment}.
#'
#' Note that datasets can be converted to class \code{xover_data} using
#' \code{\link[xover]{as_xover_data}}.
#'
#' @param data A \code{\link[tibble]{tibble}} of class \code{xover_data}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[tibble]{tibble}} of class \code{xover_data}.
#' Specifically, the input dataset \code{data} with the addition of a new column
#' \code{carryover}, stored as a factor with the same levels as
#' \code{data$treatment}. Note that if \code{data$carryover} is already present
#' it will be overwritten.
#' @references Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @seealso \code{\link[xover]{as_xover_data}} for converting datasets to class
#' \code{xover_data}.
#' @export
add_carryover_data <- function(data, summary = T) {

  ##### Input checking #########################################################

  check_data(data)
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the addition of carryover information...")
  }
  labels             <- levels(data$treatment)
  carryover          <- numeric(nrow(data))
  for (i in 1:nrow(data)) {
    if (data$period[i] == 1) {
      carryover[i]   <- labels[1]
    } else {
      data_i         <-
        dplyr::filter(data, subject == data$subject[i] &
                        period == as.numeric(as.character(data$period[i])) - 1)
      if (nrow(data_i) == 0) {
        carryover[i] <- NA
      } else {
        allocation   <- strsplit(as.character(data$sequence[i]),
                                 split = "")[[1]]
        carryover[i] <- allocation[as.numeric(as.character(data$period[i])) - 1]
      }
    }
  }
  data$carryover     <- factor(carryover, labels)
  if (summary) {
    message("...completed the addition of carryover information. Preparing",
            " outputs...")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(data)

}
