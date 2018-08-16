#' Complete case data
#'
#' Reduces a cross-over dataset to complete case form.
#'
#' \code{complete_case_data()} supports the reduction of a cross-over dataset
#' to those outcomes corresponding to subjects with no missing data. Precisely,
#' a dataset of class \code{xover_data} is provided (see \code{data}), along
#' with an outcome variable within this dataset (see \code{outcome}), and the
#' rows corresponding to subjects for whom at least one outcome variable is
#' missing for the chosen outcome variable, based on an assessment of
#' \code{data$sequence}, are removed.
#'
#' Note that datasets can be converted to class \code{xover_data} using
#' \code{\link[xover]{as_xover_data}}.
#'
#' @param data A \code{\link[tibble]{tibble}} of class \code{xover_data}.
#' @param outcome The chosen outcome variable from \code{data} upon which the
#' complete case reduction will be based. Should be stated absolutely, i.e., not
#' as a \code{\link[base]{character}} string.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[tibble]{tibble}} of class \code{xover_data}.
#' Specifically, the input dataset \code{data} reduced to its complete case
#' form.
#' @references Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @seealso \code{\link[xover]{as_xover_data}} for converting datasets to class
#' \code{xover_data}.
#' @export
complete_case_data <- function(data, outcome, summary = T) {

  ##### Input checking #########################################################

  if (missing(outcome)) {
    stop("outcome must be supplied.")
  } else {
    outcome <- substitute(outcome)
  }
  check_data(data, outcome, output = F)
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the required calculations...")
  }
  subjects    <- unique(data$subject)
  drop        <- NULL
  for (s in subjects) {
    data_s    <- dplyr::filter(data, subject == s)
    if (nrow(data_s) < length(strsplit(as.character(data_s$sequence[1]),
                                       split = "")[[1]])) {
      drop    <- c(drop, s)
    } else if (any(is.na(eval(outcome, data_s)))) {
      drop    <- c(drop, s)
    }
  }
  data        <- dplyr::filter(data, !(subject %in% drop))
  if (summary) {
    message("...completed the required calculations. Preparing outputs...")
  }
  class(data) <- c(class(data), "xover_data")

  ##### Ouputting ##############################################################

  if (summary) {
    message("...outputting.")
  }
  return(data)

}
