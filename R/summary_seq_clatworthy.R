#' Available cross-over designs from Clatworthy (1973)
#'
#' Summarises the cross-over designs available from Clatworthy (1973) via the
#' function \code{\link[xover]{seq_clatworthy}}.
#'
#' \code{summary_seq_clatworthy()} supports the determination of which designs
#' are available from \code{\link[xover]{seq_clatworthy}}. Precisely, values
#' for the number of treatments (see \code{D}), periods (see \code{J}), and
#' sequences (see \code{K}) desired in the design is specified. Then, the
#' function returns which compatible designs are supported by
#' \code{\link[xover]{seq_clatworthy}}.
#'
#' @param D The number of treatments. If specified, must be a single
#' \code{\link[base]{numeric}} integer greater than or equal to two. Defaults to
#' missing, which means no subsetting of the available designs is performed
#' based on the number of treatments.
#' @param J The number of periods. If specified, must be a
#' \code{\link[base]{numeric}} vector, containing integer values greater than or
#' equal to two. Defaults to missing, which means no subsetting of the available
#' designs is performed
#' based on the number of periods.
#' @param K The number of sequences. If specified, must be a
#' \code{\link[base]{numeric}} vector, containing integer values greater than or
#' equal to two. Defaults to missing, which means no subsetting of the available
#' designs is performed based on the number of sequences.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return A \code{\link[tibble]{tibble}} containing the available designs from
#' \code{\link[xover]{seq_clatworthy_williams}} for the chosen values of
#' \code{D}, \code{J}, and \code{K}.
#' @examples
#' # Available designs for six treatments from Clatworthy (1973)
#' clatworthy_D6     <- summary_seq_clatworthy(D = 6)
#' # Available designs for six treatments and three periods
#' clatworthy_D6J3   <- summary_seq_clatworthy(D = 6, J = 3)
#' # Available designs for six treatments, three periods, and eight sequences
#' clatworthy_D6J3K8 <- summary_seq_clatworthy(D = 6, J = 3, K = 8)
#' # Returning in tibble form
#' @references Clatworthy WH, Cameron JM, Speckman JA (1973) Tables of
#' two-associate-class partially balanced designs. \emph{US Government Printing
#' Office}.
#' @seealso \code{\link[xover]{summary_seq_clatworthy}}.
#' @export
summary_seq_clatworthy <- function(D, J, K, summary = T) {

  ##### Input checking #########################################################

  if (!missing(D)) {
    check_integer_range(D, "D", c(1, Inf), 1)
  }
  if (!missing(J)) {
    check_integer_range(J, "J", c(1, Inf), "any")
  }
  if (!missing(K)) {
    check_integer_range(K, "K", c(1, Inf), "any")
  }
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the identification of supported designs...")
  }
  internal_clatworthy_designs   <- xover:::clatworthy_designs
  if (!missing(D)) {
    internal_D                  <- D
    internal_clatworthy_designs <- dplyr::filter(internal_clatworthy_designs,
                                                 D == internal_D)
  }
  if (!missing(J)) {
    internal_J                  <- J
    internal_clatworthy_designs <- dplyr::filter(internal_clatworthy_designs,
                                                 J == internal_J)
  }
  if (!missing(K)) {
    internal_K                  <- K
    internal_clatworthy_designs <- dplyr::filter(internal_clatworthy_designs,
                                                 K == internal_K)
  }
  if (summary) {
    message("...identified all supported designs. Preparing outputs...")
  }

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(internal_clatworthy_designs)

}
