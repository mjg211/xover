#' All compatible treatment combination cross-over design specification
#'
#' Specifies cross-over designs based on all possible compatible treatment
#' combinations.
#'
#' \code{seq_combs()} supports the specification of designs based on all
#' possible compatible treatment combinations for a given number of treatments
#' and periods. Designs for any number of treatments (see \code{D}) and
#' periods (see \code{J}) are supported, for any chosen treatment labels (see
#' \code{labels}). In addition, the designs can be returned in
#' \code{\link[base]{matrix}} or \code{\link[tibble]{tibble}} form (see
#' \code{as_matrix}).
#'
#' Precisely, all possible combinations of
#' \ifelse{html}{\out{<i>D</i>}}{\eqn{D}} treatments across
#' \ifelse{html}{\out{<i>J</i>}}{\eqn{J}} periods are determined, subject to the
#' specified restrictions on the maximal number of times a treatment can appear
#' in a sequence (see \code{max_occurrences}), and the maximal number of
#' sequential periods a treatment can be repeatedly allocated in (see
#' \code{max_repetitions}). Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' cross-over design matrix corresponds to the treatment a subject on the
#' \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to three. Defaults
#' to \code{2}.
#' @param J The number of periods. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to two. Defaults
#' to \code{D}.
#' @param max_occurrences The maximal number of times a treatment can be
#' allocated in any sequence. Must be a single \code{\link[base]{numeric}}
#' integer between one and \code{J} inclusive. Defaults to \code{1}.
#' @param max_repetititions The maximal number of times a treatment can be
#' repeatedly allocated in any sequence. Must be a single
#' \code{\link[base]{numeric}} integer between one and \code{max_occurrences}
#' inclusive. Defaults to \code{1}.
#' @param labels A \code{\link[base]{vector}} of labels for the treatments.
#' Should be of \code{\link[base]{length}} \code{D}, containing unique elements.
#' Defaults to \code{0:(D - 1)}.
#' @param as_matrix A \code{\link[base]{logical}} variable indicating whether
#' the design should be returned as a \code{\link[base]{matrix}}, or a
#' \code{\link[tibble]{tibble}}. Defaults to \code{T}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return \code{\link[base]{NULL}} if no compatible sequence exists.
#' Otherwise, a \code{\link[base]{matrix}} if \code{as_matrix = T} (with
#' rows corresponding to sequences and columns to periods), or a
#' \code{\link[tibble]{tibble}} if \code{as_matrix = F} (with rows corresponding
#' to a particular period on a particular sequence). In either case, the
#' returned object will have class \code{xover_seq}.
#' @examples
#' # Two-treatment two-period and three-treatment three-period designs, with
#' # maximal one allocation to a treatment
#' two_by_two            <- seq_combs(D = 2)
#' three_by_three        <- seq_combs(D = 3)
#' # Using different labels
#' two_by_two_AB         <- seq_combs(D = 2, labels = c("A", "B"))
#' three_by_three_ABC    <- seq_combs(D = 3, labels = c("A", "B", "C"))
#' # Returning in tibble form
#' two_by_two_tibble     <- seq_combs(D = 2, as_matrix = F)
#' three_by_three_tibble <- seq_combs(D = 3, as_matrix = F)
#' @references Jones B, Kenward MG (2014) \emph{Design and Analysis of
#' Cross-Over Trials}. Chapman and Hall: London, 3rd Edition.
#' @author Based on code from the \code{\link[crossdes]{crossdes}} package by
#' Oliver Sailer.
#' @export
seq_combs <- function(D = 2, J = D, max_occurrences = 1, max_repetitions = 1,
                      labels = 0:(D - 1), as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(1, Inf), 1)
  check_integer_range(J, "J", c(1, Inf), 1)
  check_integer_range(max_occurrences, "max_occurrences", c(0, J + 1), 1)
  if (J > D) {
    if (max_occurrences == 1) {
      stop("When J > D max_occurrences must be greater than one.")
    }
  }
  check_integer_range(max_repetitions, "max_repetitions",
                      c(0, max_occurrences + 1), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  if (max_occurrences == 1) {
    sequences           <- iterpc::getall(iterpc::iterpc(n = D, r = J,
                                                         labels = 0:(D - 1),
                                                         ordered = T,
                                                         replace = F))
  } else {
    sequences           <- iterpc::getall(iterpc::iterpc(n = D, r = J,
                                                         labels = 0:(D - 1),
                                                         ordered = T,
                                                         replace = T))
    keep                <- rep(T, nrow(sequences))
    for (k in 1:nrow(sequences)) {
      d                 <- 0
      while (all(keep[k] == T, d <= D - 1)) {
        which_d         <- which(sequences[k, ] == d)
        len_which_d     <- length(which_d)
        if (len_which_d > max_occurrences) {
          keep[k]       <- F
        } else if (all(len_which_d >= max_repetitions, len_which_d > 1)) {
          if (max_repetitions == 1) {
            if (any(which_d[2:len_which_d] ==
                      which_d[1:(len_which_d - 1)] + 1)) {
              keep[k]   <- F
            }
          } else {
            counter     <- 1
            for (i in 2:len_which_d) {
              if (which_d[i] == which_d[i - 1] + 1) {
                counter <- counter + 1
              } else {
                counter <- 1
              }
              if (counter > max_repetitions) {
                keep[k] <- F
                break
              }
            }
          }
        }
        d               <- d + 1
      }
    }
    sequences           <- sequences[keep, , drop = F]
  }
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  if (nrow(sequences) > 0) {
    sequences           <- convert_labels(sequences, D, labels, 0:(D - 1))
    sequences           <- transform_to_xover(sequences, labels, as_matrix)
  } else {
    sequences           <- NULL
  }

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
