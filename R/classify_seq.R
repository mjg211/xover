#' Cross-over design classification
#'
#' Classifies a cross-over design.
#'
#' \code{classify_seq()} supports the classification of a cross-over design (see
#' \code{sequences}). Ultimately, the design will be assigned in to one of
#' twelve possible categories. Briefly, a design is said to be a balanced block
#' design if the following three conditions hold:
#' \itemize{
#' \item Each treatment appears equally often in the design;
#' \item The design is binary in the sense that each treatment appears in each
#' block either \ifelse{html}{\out{<i>n</i>}}{\eqn{n}} or
#' \ifelse{html}{\out{<i>n</i> + 1}}{\eqn{n + 1}} times, where
#' \ifelse{html}{\out{<i>n</i>}}{\eqn{n}} is an integer;
#' \item The number of concurrences of treatments
#' \ifelse{html}{\out{<i>d</i><sub>1</sub>}}{\eqn{d_1}} and
#' \ifelse{html}{\out{<i>d</i><sub>2</sub>}}{\eqn{d_2}} is the same for all
#' pairs of distinct treatment
#' \ifelse{html}{\out{(<i>d</i><sub>1</sub>, <i>d</i><sub>2
#' </sub>)}}{\eqn{(d_1,d_2)}}.
#' }
#' Here, the blocks are either rows or columns. A design that has less columns
#' (rows) than treatments is said to be incomplete with respect to rows
#' (columns). A design that is balanced with respect to both rows and columns is
#' called a generalized Youden design (GYD). A GYD for which each treatment
#' occurs equally often in each row (column) is called uniform on the rows
#' (columns). If both conditions hold, it is called a generalized Latin Square.
#' A design where each treatment occurs exactly once in each row and column is
#' called a Latin Square. See the package vignette for details on these
#' potential classifications.
#'
#' @param sequences An object of \code{\link[base]{class}} \code{xover_seq}; a
#' set of cross-over design sequences.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress, and the identified classification, should
#' be printed to the console. Defaults to \code{T}.
#' @return A \code{\link[base]{list}} containing the following components
#' \itemize{
#' \item \code{classification}: A \code{\link[base]{character}} string
#' describing the classification of the specified cross-over design.
#' \item \code{occurrences}: A \code{\link[base]{matrix}} describing the
#' treatment occurrences.
#' \item \code{col_incidences}: A \code{\link[base]{matrix}} describing the
#' treatment column incidences.
#' \item \code{row_incidences}: A \code{\link[base]{matrix}} describing the
#' treatment row incidences.
#' \item \code{col_concurrences}: A \code{\link[base]{matrix}} describing the
#' treatment column concurrences.
#' \item \code{row_concurrences}: A \code{\link[base]{matrix}} describing the
#' treatment row concurrences.
#' }
#' @examples
#' # Classify a three-treatment three-period Latin Square
#' latin    <- seq_latin(D = 3)
#' classify <- classify_seq(latin)
#' @author Based on code from the \code{\link[crossdes]{crossdes}} package by
#' Oliver Sailer.
#' @seealso The \code{seq_#()} functions within \code{\link[xover]{xover}}.
#' @export
classify_seq <- function(sequences, summary = T) {

  ##### Input checking #########################################################

  check_sequences(sequences)
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the required calculations...")
  }
  if (tibble::is_tibble(sequences)) {
    sequences      <- tibble_to_matrix(sequences)
  }
  J                <- ncol(sequences)
  K                <- nrow(sequences)
  labels           <- unique(as.vector(sequences))
  rows             <- internal_classify_seq(sequences)
  cols             <- internal_classify_seq(t(sequences))
  check            <- T
  if (all(rows$type[c(1:3, 5)], cols$type[c(1:3, 5)])) {
    classification <- "latin square"
  } else if (all(rows$type[c(1:3, 6)], cols$type[c(1:3, 6)])) {
    classification <- "generalized latin square"
  } else if (all(rows$type[c(1:3, 6)], cols$type[1:3])) {
    classification <- paste("regular generalized Youden design that is uniform",
                            "on the rows")
  } else if (all(rows$type[1:3], cols$type[c(1:3, 6)])) {
    classification <- paste("regular generalized Youden design that is uniform",
                            "on the columns")
  } else if (all(rows$type[1:3], cols$type[1:3])) {
    classification <- "generalized Youden design"
  } else if (all(rows$type[c(1:3, 5)])) {
    classification <- "balanced complete block design WRT rows"
  } else if (all(rows$type[c(1:3, 4)])) {
    classification <- "balanced incomplete block design WRT rows"
  } else if (all(rows$type[c(1:3)])) {
    classification <- "balanced block design WRT rows"
  } else if (all(cols$type[c(1:3, 5)])) {
    classification <- "balanced complete block design WRT columns"
  } else if (all(cols$type[c(1:3, 4)])) {
    classification <- "balanced incomplete block design WRT columns"
  } else if (all(cols$type[c(1:3)])) {
    classification <- "balanced block design WRT columns"
  } else {
    classification <- "neither balanced WRT rows nor WRT columns"
    check          <- F
  }
  if (summary) {
    if (summary) {
      message("...completed the required calculations.\n")
    }
    if (check) {
      message("   The design is a ", classification, ".\n")
    } else {
      message("   The design is ", classification, ".\n")
    }
  }
  if (summary) {
    message("   Preparing outputs...")
  }
  names(rows$occurrences)       <- rownames(rows$incidences)   <-
    rownames(cols$incidences)   <- colnames(rows$concurrences) <-
    rownames(rows$concurrences) <- colnames(cols$concurrences) <-
    rownames(cols$concurrences) <- paste("d =", labels)
  colnames(rows$incidences)     <- paste("k =", 1:K)
  colnames(cols$incidences)     <- paste("j =", 1:J)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(list(classification   = classification,
              occurrences      = rows$occurrences,
              col_incidences   = cols$incidences,
              row_incidences   = rows$incidences,
              col_concurrences = cols$concurrences,
              row_concurrences = rows$concurrences))

}
