#' Mutually Orthogonal Latin Square cross-over design specification
#'
#' Specifies either a complete set of Mutually Orthogonal Latin Squares (MOLS),
#' or a single cross-over design based on a complete set of MOLS.
#'
#' \code{seq_mols()} supports the specification of either a complete set of
#' MOLS, or a single cross-over design based on such a set (see \code{type}).
#' Designs for any prime power number of treatments (see \code{D}) are
#' supported, for any chosen treatment labels (see \code{labels}). In
#' addition, the designs can be returned in \code{\link[base]{matrix}} or
#' \code{\link[tibble]{tibble}} form (see \code{as_matrix}). When a single
#' cross-over design is to be returned, the number of periods can be any integer
#' between two and \code{D}. Ultimately,
#' the \ifelse{html}{\out{(<i>k</i>,<i>j</i>)}}{\eqn{(k,j)}}th
#' element of the cross-over design matrix/matrices corresponds to the treatment
#' a subject on the \ifelse{html}{\out{<i>k</i>}}{\eqn{k}}th sequence would
#' receive in the \ifelse{html}{\out{<i>j</i>}}{\eqn{j}}th period.
#'
#' The complete set of MOLS is constructed using Galois fields. In the case that
#' \code{type = "set"}, \ifelse{html}{\out{<i>D</i> - 1}}{\eqn{D - 1}} Latin
#' Squares of order \ifelse{html}{\out{<i>D</i>}}{\eqn{D}} will be constructed.
#' They will be mutually orthogonal, i.e., if any two of them are super-imposed,
#' the resulting array will contain each ordered pair
#' \ifelse{html}{\out{(<i>d</i><sub>1</sub>,<i>d</i><sub>2
#' </sub>)}}{\eqn{(d_1,d_2)}} exactly once. The Latin Squares will be in
#' standard order, i.e., the first sequence is always equal to the specified
#' treatment labels. Otherwise, when \code{type = "single"}, the resulting
#' design will have \ifelse{html}{\out{<i>D</i>(<i>D</i> - 1)}}{\eqn{D(D - 1)}}
#' sequences, and each treatment will appear in each sequence at most once. The
#' design will be a generalized Youden design that is also balanced for
#' carryover effects.
#'
#' @param D The number of treatments. Must be a single
#' \code{\link[base]{numeric}} prime power between three and 100. Defaults to
#' \code{3}.
#' @param type Must be either \code{"set"} or \code{"single"}, indicating
#' whether a complete set of MOLS, or a single cross-over design based on such a
#' complete set, should be returned. Defaults to \code{"set"}.
#' @param J When \code{type = "single"}, this will determine
#' the number of periods in the resulting design. Must be a single
#' \code{\link[base]{numeric}} integer between two and \code{D} inclusive.
#' Defaults to \code{D}.
#' @param labels A \code{\link[base]{vector}} of labels for the treatments.
#' Should be of \code{\link[base]{length}} \code{D}, containing unique elements.
#' Defaults to \code{0:(D - 1)}.
#' @param as_matrix A \code{\link[base]{logical}} variable indicating whether
#' the design should be returned as a \code{\link[base]{matrix}}, or a
#' \code{\link[tibble]{tibble}}. Defaults to \code{T}.
#' @param summary A \code{\link[base]{logical}} variable indicating whether a
#' summary of the function's progress should be printed to the console. Defaults
#' to \code{T}.
#' @return If \code{type = "single"}: Either a \code{\link[base]{matrix}} if
#' \code{as_matrix = T} (with rows corresponding to sequences and columns to
#' periods), or a \code{\link[tibble]{tibble}} if \code{as_matrix = F} (with
#' rows corresponding to a particular period on a particular sequence).
#' Otherwise, if \code{type = "set"}, a list consisting of either matrices or
#' tibbles, again dependent on the value of \code{as_matrix}. In either case,
#' the returned object(s) will have class \code{xover_seq}.
#' @examples
#' # A complete set of MOLS for three treatments
#' mols_set    <- seq_mols()
#' # A corresponding cross-over design, based on a complete set of MOLS
#' mols_single <- seq_mols(type = "single")
#' @references Wakeling IN, MacFie HJH (1995) Designing consumer trials balanced
#' for first and higher orders of carry-over effect when only a subset of k
#' samples from t may be tested. \emph{Food Qual Prefer} \strong{6:}299-308.
#' @references Williams EJ (1949) Experimental designs balanced for the
#' estimation of residual effects of treatments. \emph{Aust J Sci Res Ser A}
#' \strong{2:}149-168.
#' @author Based on functions from the \code{\link[crossdes]{crossdes}} package
#' by Oliver Sailer.
#' @export
seq_mols <- function(D = 4, type = "set", J = D, labels = 0:(D - 1),
                     as_matrix = T, summary = T) {

  ##### Input checking #########################################################

  prime_powers <- c(3, 4, 5, 7, 8, 9, 11, 13, 16, 17, 19, 23, 25, 27, 29, 31,
                    32, 37, 41, 43, 47, 49, 53, 59, 61, 64, 67, 71, 73, 79, 81,
                    83, 89, 97)
  check_belong(D, "D", prime_powers, 1)
  check_belong(type, "type", c("set", "single"), 1)
  if (type == "single") {
    check_integer_range(J, "J", c(1, D + 1), 1)
  } else {
    if (J != D) {
      warning("J has been changed from default: this will have no effect when ",
              "type = \"set\".")
    }
  }
  check_labels(labels, D)
  check_logical(as_matrix, "as_matrix")
  check_logical(summary, "summary")

  ##### Main computations ######################################################

  if (summary) {
    message("   Beginning the design specification...")
  }
  prime_fact <- matrix(c(3, 2, 5, 7, 2, 3, 11, 13, 2, 17, 19, 23, 5, 3, 29, 31,
                         2, 37, 41, 43, 47, 7, 53, 59, 61, 2, 67, 71, 73, 79, 3,
                         83, 89, 97, 1, 2, 1, 1, 3, 2, 1, 1, 4, 1, 1, 1, 2, 3,
                         1, 1, 5, 1, 1, 1, 1, 2, 1, 1, 1, 6, 1, 1, 1, 1, 4, 1,
                         1, 1), ncol = 2)
  pn         <- prime_fact[prime_powers == D, ]
  p          <- pn[1]
  n          <- pn[2]
  sequences  <- list()
  if (n == 1) {
    f        <- 0:(p - 1)
    for (m in 1:(p - 1)) {
      sequences[[m]]           <- matrix(0, D, D)
      for (i in 1:p) {
        for (j in 1:p) {
          dummy                <- (f[m + 1] * f[i] + f[j])%%p
          sequences[[m]][i, j] <- dummy + 1
        }
      }
    }
  } else {
    prim_pol <- gf(p, n)[[2]][1, ]
    f        <- factor_comb(p, n)
    for (m in 1:(D - 1)) {
      sequences[[m]]           <- matrix(0, D, D)
      for (i in 1:D) {
        for (j in 1:D) {
          dummy                <- mult(f[m + 1, ], f[i, ]) +
                                    c(numeric(n - 1), f[j, ])
          dummy                <- (redu(dummy, prim_pol))%%p
          a                    <- 0
          for (r in 1:(n - 1)) {
            a                  <- a + dummy[r]*(p^(n - r))
          }
          sequences[[m]][i, j] <- a + dummy[n] + 1
        }
      }
    }
  }
  if (summary) {
    message("...completed the design specification. Preparing outputs...")
  }
  if (type == "set") {
    for (m in 1:(D - 1)) {
      sequences[[m]] <- convert_labels(sequences[[m]], D, labels, 1:D)
      sequences[[m]] <- transform_to_xover(sequences[[m]], labels, as_matrix)
    }
  } else {
    new_sequences    <- NULL
    for (m in 1:(D - 1)) {
      new_sequences  <- rbind(new_sequences, t(sequences[[m]]))
    }
    sequences        <- new_sequences
    sequences        <- convert_labels(sequences, D, labels, 1:D)
    sequences        <- transform_to_xover(sequences, labels, as_matrix)
  }

  ##### Outputting #############################################################

  if (summary) {
    message("...outputting.")
  }
  return(sequences)

}
