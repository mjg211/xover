#' Balanced Incomplete Block cross-over design generation
#'
#' Searches for a Balanced Incomplete Block (BIB) cross-over design.
#'
#' \code{seq_bib()} uses the function \code{\link[AlgDesign]{optBlock}} from the
#' \code{\link[AlgDesign]{AlgDesign}} package to search for a BIB design.
#' Subject to the conditions described below, any number of sequences (see
#' \code{K}), for any number of treatments (see \code{D}) and periods (see
#' \code{J}) are supported, for any chosen treatment labels (see \code{labels}).
#' In addition, the designs can be returned in \code{\link[base]{matrix}} or
#' \code{\link[tibble]{tibble}} form (see \code{as_matrix}).
#'
#' Precisely, the function \code{\link[AlgDesign]{optBlock}} tries to find a
#' D-optimal block design for the specified parameters. The resulting design
#' need not be a BIB design. The necessary conditions for the existence are that
#' \ifelse{html}{\out{<i>JK</i>/<i>D</i>}}{\eqn{JK/D}} and
#' \ifelse{html}{\out{<i>JK</i>(<i>J</i> - 1)/(<i>D</i>(<i>D</i> -
#' 1))}}{\eqn{JK(J - 1)/(D(D - 1))}}: these are therefore enforced by the
#' function. However, even if they are fulfilled, there need not be a BIB
#' design. If no BIB design is found, the call to
#' \code{\link[AlgDesign]{optBlock}} is iterated. If no BIB design is found
#' after \code{max_iter} iterations, the search is terminated. The resulting
#' design should be checked by the user by applying the function
#' \code{\link[xover]{classify_seq}}. Ultimately, the
#' \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th element of the
#' cross-over design matrix corresponds to the treatment a subject on the
#' \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would receive in the
#' \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to three. Defaults
#' to \code{3}.
#' @param J The number of periods. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to two. Defaults to
#' \code{2}.
#' @param K The number of sequences. Must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to two. Defaults to
#' \code{12}.
#' @param labels A \code{\link[base]{vector}} of labels for the treatments.
#' Should be of \code{\link[base]{length}} \code{D}, containing unique elements.
#' Defaults to \code{0:(D - 1)}.
#' @param as_matrix A \code{\link[base]{logical}} variable indicating whether
#' the design should be returned as a \code{\link[base]{matrix}}, or a
#' \code{\link[tibble]{tibble}}. Defaults to \code{T}.
#' @param max_iter The maximum allowed number of iterations in the search
#' procedure. Must be a single \code{\link[base]{numeric}} integer greater than
#' or equal to one. Passed to \code{\link[AlgDesign]{optBlock}}. Defaults to
#' \code{100}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return Either a \code{\link[base]{matrix}} if \code{as_matrix = T} (with
#' rows corresponding to sequences and columns to periods), or a
#' \code{\link[tibble]{tibble}} if \code{as_matrix = F} (with rows corresponding
#' to a particular period on a particular sequence). In either case, the
#' returned object will have class \code{xover_seq}.
#' @examples
#' # A three-treatment two-period twelve-sequence design
#' bib        <- seq_bib()
#' # Using different labels
#' bib_ABC   <- seq_bib(labels = LETTERS[1:3])
#' # Returning in tibble form
#' bib_tibble <- seq_bib(as_matrix = F)
#' # Check that the design is a BIB design
#' check      <- classify_seq(bib)
#' @references Wheeler RE (2014) \emph{AlgDesign: Algorithmic Experimental
#' Design}. Version 1.1-7.3. URL:
#' \url{https://CRAN.R-project.org/package=AlgDesign}.
#' @author Based on code from the \code{\link[crossdes]{crossdes}} package by
#' Oliver Sailer.
#' @export
seq_bib <- function(D = 3, J = 2, K = 12, labels = 0:(D - 1), as_matrix = T,
                    max_iter = 100, summary = T) {

  ##### Input checking #########################################################

  check_integer_range(D, "D", c(2, Inf), 1)
  check_integer_range(J, "J", c(1, D), 1)
  check_integer_range(K, "K", c(1, Inf), 1)
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_integer_range(max_iter, "max_iter", c(0, Inf), 1)
  check_logical(summary, "summary")
  if ((J*K/D)%%1 != 0) {
    stop("JK/D must be an integer for a BIB design to potentially exist.")
  }
  if ((J*K*(J - 1)/(D*(D - 1)))%%1 != 0) {
    stop("JK(J - 1)/(D(D - 1)) must be an integer for a BIB design to ",
         "potentially exist.")
  }

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  iter             <- 0
  is_bib           <- F
  while (all(!is_bib, iter < max_iter)) {
    sequences_iter <- AlgDesign::optBlock(~., withinData = factor(1:D),
                                          blocksizes = rep(J, K))
    sequences      <- matrix(sequences_iter$rows, byrow = TRUE, ncol = J)
    is_bib         <- all(internal_classify_seq(sequences)$type[1:4])
    iter           <- iter + 1
  }
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  sequences        <- convert_labels(sequences, D, labels, 1:D)
  sequences        <- transform_to_xover(sequences, labels, as_matrix)

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
